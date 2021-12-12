type int_bigarray = (int,Bigarray.int_elt,Bigarray.c_layout)Bigarray.Array1.t
type bigarray_int = int_bigarray
type bigstring = Bigstringaf.t

module Times = struct
  open Sexplib.Std
  type t = { atim:float; mtim:float }[@@deriving sexp]
  (* NOTE at the moment we only modify mtim *)
end

module type MONAD = sig
  type 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
end

module type BLK = sig
  type ctxt

  type blk = bigstring

  val create: ?clean:char -> ctxt -> blk
  val blk_sz: blk -> int
  val fill: char -> blk -> unit


  type blk_int = int_bigarray

  val create_int: ?clean:int -> ctxt -> blk_int
  val blk_sz_int: blk_int -> int
  val fill_int: int -> blk_int -> unit

  (* Conversions *)
  val to_blk_int : blk -> blk_int
  val to_blk     : blk_int -> blk
end

module type BLKDEV = sig
  type ctxt

  type 'a m

  type blk = bigstring

  type blk_id (* = int, with -1 representing "None" *)

  val blk_id_none : blk_id 
  (** -1 represents "None" *)

  val blk_id_is_some: blk_id -> bool
  (** Typically blk_ids >=0 are considered valid; we assume blk_ids
     passed to functions are "some", unless otherwise indicated;
     blk_ids stored on disk may be some or none, so we should check
     each if we are not sure, before doing anything further; for
     efficiency and ease of interaction with int arrays, we prefer not
     to use an option type here *)


  type t

  (** A blk is normally a bigstring, but we can cast to a bigarray of
     int; the underlying piece of memory is shared of course, so these
     casts happen purely at the type level and have no runtime cost or
     significance *)

  val write: t -> blk_id -> blk -> unit m
  val read : t -> blk_id -> blk m  

  val sync : unit -> unit m
end


(** The freelist is in charge of its own syncing (so, no sync
   function), and guarantees to always be consistent (allocated blocks
   are recorded on-disk as allocated, although as an optimization some
   recently freed blocks may not be recorded as freed, introducing the
   possibility of lost blocks on crash *)
module type FREELIST = sig

  type 'a m
  type blk_id

  type t (* = { hd:blk_id } *)

  val create     : blk_id -> t m
  val open_      : blk_id -> t option m

  (* FIXME this should just return blk_id, without even writing to the store *)
  val alloc      : t -> blk_id m
  val free       : t -> blk_id -> unit m

  val alloc_many : t -> int -> blk_id list m
  val free_many  : t -> blk_id list -> unit m  

  val debug_used_blks  : t -> blk_id list m
  (** The blks used by the freelist itself *)

end

(* Root blk; the blk_id is 0 *)
module type BLK0 = sig
  type blk_id = int
  type t = { freelist_root: blk_id; prefiles: blk_id array; (* ... *) }
  type 'a m

  val read  : unit -> t m
  val write : t -> unit m
  
end

(** A map from int to int; heavily cached; basis for BLK_MAP *)
module type INT_MAP = sig

  type 'a m
  type t
  type blk_id
  type ctxt

  type k := int
  type v := int

  (* FIXME if we have ctxt, we might prefer to get init blk_id from freelist; so make blk_id optional? *)
  val create   : ctxt -> blk_id -> t m
  val open_    : ctxt -> blk_id -> t option m

  val root     : t -> blk_id


  val height   : t -> int  (** height of the underlying tree on disk *)

  val find_opt : t -> k -> v option m
  val replace  : t -> k -> v -> unit m
  val delete   : t -> k -> unit m

  val sync     : t -> unit m

  
  val destroy  : t -> unit m
  (** Free underlying blks *)

  val debug_used_blks: t -> blk_id list m
end


(** A map from blk index to blk_id, maintained on disk and mirrored in
   memory *)
module type BLK_MAP = sig
  type 'a m
  type blk_id
  type ctxt

  type t 

  val create: ctxt -> blk_id -> t m
  val open_ : ctxt -> blk_id -> t option m

  val root : t -> blk_id

  val height: t -> int  (** height of the underlying tree on disk *)

  val set_blkid: t -> int -> blk_id -> unit m
  (** associate blk_id with a blk index in the file; results in an
     error if there is already a blk associated with the id *)

  val get_blkid: t -> int -> blk_id option m
  (** given a blk index, return the corresponding blk_id *)

  val sync : t -> unit m
end


(** The kinds of filesystem objects, and their associated metadata *)
module type KIND = sig
  type 'a m
  type blk_id

  type file_meta
  type dir_meta

  type ctxt

  (** NOTE this must be sure to go via the in-memory version of the
     object if it exists; FIXME do we prefer inodes here? paths? *)
  val stat : ctxt -> blk_id -> [`F of file_meta | `D of dir_meta ] option m
end


(** A file maintains a map from blk index to blk_id, a sz, and
   possibly other metadata *)
module type FILE = sig
  type 'a m
  type blk
  type blk_id
  type ctxt

  type t (* = { blk_map:Blk_map.t; sz:int }; on disk we have a blk
            that has a pointer to the blk_map, a sz field, and
            possibly some other metadata *)


  val create       : ctxt -> blk_id -> t m
  (** Create a new file, rooted at the given blk_id *)

  val open_        : ctxt -> blk_id -> t option m

  val read_blk     : t -> int -> blk m
  (** This will allocate a new blk_id if needed *)

  val write_blk    : t -> int -> blk -> unit m
  (** This will allocate a new blk_id if needed *)

(*
  (* val used_blks : t -> blk_id list m *)
  val reveal_blk   : t -> int -> (blk_id * blk * int) m
  (** Given a position in the file, reveal the underlying blk and the
      position within the blk (presumably for further read/write) *)
*)

  val pread        : t -> off:int -> len: int -> bigstring m

  (* use bigstring slice rather than off, len *)
  val pwrite       : t -> src:bigstring -> dst_off:int -> unit m

  val read         : t -> pos:int ref -> len:int -> bigstring m
  (** Starting from the pos, read exactly len bytes (unless there are
     less than len available, in which case read as much as possible);
     update pos as a side effect FIXME perhaps the caller should
     provide the bytes, so they can be reused in future calls *)

  val write        : t -> pos:int ref -> src:bigstring -> unit m
  (** Starting from pos, write all the bytes given; update pos as a
     side effect *)

  val get_sz       : t -> int
  (* val set_sz    : int -> unit not in public intf *)

  val get_times    : t -> Times.t

  val sync         : t -> unit m
end


(*
(** A small file can fit sz and data into a single block; first 4
   bytes store length of string; rest is data *)
module type SMALL_FILE = sig
  type t 
  type 'a m

  val get_data: t -> string

  val set_data: string -> t
  (** Will only allow shortish strings eg of length 4092 bytes or less *)
  
  val sync: t -> unit m
end
*)

(** A directory maps names (strings, max len 256) to filesystem
   objects (files or dirs).

The implementation of dirs is just a list (file) of (k,v) entries,
   with an additional (name-hash -> offset) map for fast
   find/replace. Deleting an entry just invalidates the value at the
   given offset. This means that directories can accumulate lots of
   garbage if many names are added then deleted. As an optimization,
   if all entries are deleted we can perhaps reclaim the entries list
   and the map.

Actually, we also record various other things in a directory such as:
   the number of valid entries (so we can GC when 0), and the parent
   directory, and other metadata.  *)
module type DIR = sig
  type 'a m
  type blk_id
  type ctxt

  type t
  type k = string
  type v 

  type dh 
  (** dh = dir_handle *)

  (* NOTE the test FS tracks which kinds of objects are associated
     with which blk_ids, and attempting to open an object at a blk_id
     with the incorrect type will return none; for create, we expect
     that most uses will use a fresh blk_id; if this is not the case
     an exception should be thrown *)

  (* FIXME perhaps use term "anchor" to represent root block *)

  val create        : ctxt -> root:blk_id -> parent:blk_id -> t m
  val open_         : ctxt -> blk_id -> t option m
  val sync          : t -> unit m

  val find_opt      : t -> k -> v option m
  val replace       : t -> k -> v -> unit m
  val delete        : t -> k -> unit m

  val get_times     : t -> Times.t

  (** Directory handles *)

  val readdir_limit : int 
  (** The max number of entries that can be returned by a readdir call *)

  val open_dh       : t -> dh
  val readdir       : dh -> (k*v) list m
  (** This is the empty list IFF there are no more entries; otherwise
     a certain small number of entries are returned, which can safely
     be dealt with by non-tail-recursive functions *)
  val close_dh      : dh -> unit

end


(** A filesystem is then a store of directories and files. Some of the
   objects are "live" in memory, and the rest are stored on disk.

    The ctxt that is used in earlier sigs is in fact derived from this fs object.
 *)
module type FS = sig
  type 'a m
  type blk_id

  type file (* live, in memory; equal to FILE.t *)
  type dir (* live, in memory; equal to DIR.t *)

  type file_meta (* from KIND *)
  type dir_meta (* from KIND *)

  type t (* equal to Runtime_context.t *)

  (* block id that stores the root directory *)
  val root : t -> blk_id
    
  val stat: t -> blk_id -> [`F of file_meta | `D of dir_meta ] option m

  (* the following with_... functions pin the file in memory for the
     duration of f *)

  val with_file   : t -> blk_id -> f:(file -> 'a m) -> 'a m
  val with_dir    : t -> blk_id -> f:(dir -> 'a m) -> 'a m

  val create_file : t -> dir -> string -> file m
  val create_dir  : t -> parent:dir -> string -> dir m

  (* FIXME what do we do about deleting? need to count links for
     files; need to ensure parent doesn't reference a dir that we
     delete *)  
  val delete_file : t -> blk_id -> unit m
  val delete_dir  : t -> blk_id -> unit m

  (** The following should sync all file and dir objects as well *)
  val sync        : t -> unit m
end
