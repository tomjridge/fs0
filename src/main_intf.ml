type int_bigarray = (int,Bigarray.int_elt,Bigarray.c_layout)Bigarray.Array1.t
type bigarray_int = int_bigarray
type bigstring = Bigstringaf.t

module type MONAD = sig
  type 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
end

module type BLKDEV = sig
  
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

  val blk_sz: int (* size in bytes *)

  (** A blk is normally a bigstring, but we can cast to a bigarray of
     int; the underlying piece of memory is shared of course, so these
     casts happen purely at the type level and have no runtime cost or
     significance *)
  type blk_int = bigarray_int
  val to_blk_int : blk -> blk_int
  val blk_sz_int : int  (* blk_int size *)
  val to_blk     : blk_int -> blk

  val write: blk_id -> blk -> unit m
  val read : blk_id -> blk m  
    
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

  val used_blks  : t -> blk_id list m

  val alloc      : ?clean:[`Char of char | `Int of int] -> unit -> blk_id m
  val free       : blk_id -> unit

  val alloc_many : ?clean:[`Char of char | `Int of int] -> int -> blk_id list m
  val free_many  : blk_id list -> unit m  

end

(* Root blk; the blk_id is 0 *)
module type BLK0 = sig
  type blk_id = int
  type t = { freelist_root: blk_id; prefiles: blk_id array; (* ... *) }
  type 'a m

  val read: unit -> t m
  val write: t -> unit m
  
end

(** A map from int to int; heavily cached; basis for BLK_MAP *)
module type INT_MAP = sig

  type 'a m
  type t
  type blk_id

  type k := int
  type v := int

  val create: blk_id -> t

  val height: t -> int  (** height of the underlying tree on disk *)

  val find_opt : t -> k -> v option m

  val replace: t -> k -> v -> unit

  val delete : t -> k -> unit

  val sync : t -> unit m

end


(** A map from blk index to blk_id, maintained on disk and mirrored in
   memory *)
module type BLK_MAP = sig

  type t (* = { root: blk_id } *)
  type blk_id
  type 'a m

  val height: t -> int  (** height of the underlying tree on disk *)

  val set_blkid: int -> blk_id -> unit
  (** associate blk_id with a blk index in the file; results in an
     error if there is already a blk associated with the id *)

  val get_blkid: int -> blk_id option
  (** given a blk index, return the corresponding blk_id *)

  val sync_map : unit -> unit m
end


(** The kinds of filesystem objects, and their associated metadata *)
module type KIND = sig
  type 'a m
  type blk_id

  type file_meta
  type dir_meta

  (** NOTE this must be sure to go via the in-memory version of the
     object if it exists *)
  val stat : blk_id -> [`F of file_meta | `D of dir_meta ]
end


(** A file maintains a map from blk index to blk_id, a sz, and
   possibly other metadata *)
module type FILE = sig

  type t (* = { blk_map:Blk_map.t; sz:int }; on disk we have a blk
            that has a pointer to the blk_map, a sz field, and
            possibly some other metadata *)
  type blk
  type blk_id
  type 'a m

  val create    : blk_id -> t m
  (** Create a new file, rooted at the given blk_id *)

  val open_     : blk_id -> t option m

  val read_blk  : t -> int -> blk m
  (** This will allocate a new blk_id if needed *)

  val write_blk : t -> int -> blk -> unit m
  (** This will allocate a new blk_id if needed *)

  val used_blks : t -> blk_id list m

  val pread     : t -> off:int -> len: int -> bigstring m
  val pwrite    : t -> src:bigstring -> src_off:int -> src_len:int -> dst_off:int -> unit m

  val read      : t -> pos:int ref -> len:int -> bytes m
  (** Starting from the pos, read exactly len bytes (unless there are
     less than len available, in which case read as much as possible);
     update pos as a side effect *)

  val write     : t -> pos:int ref -> bytes -> unit m
  (** Starting from pos, write all the bytes given; update pos as a
     side effect *)

  val reveal_blk : t -> int -> blk_id * blk * int
  (** Given a position in the file, reveal the underlying blk and the
      position within the blk (presumably for further read/write) *)

  val get_sz    : int
  val set_sz    : int -> unit

  val sync : unit -> unit m
end



(** A small file can fit sz and data into a single block; first 4
   bytes store length of string; rest is data *)
module type SMALL_FILE = sig
  type t 
  type 'a m

  val get_data: t -> string

  val set_data: string -> t
  (** Will only allow shortish strings eg of length 4092 bytes or less *)
  
  val sync: unit -> unit m
end


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

  type t
  type k := string
  type v 

  type dh 
  (** dh = dir_handle *)

  (* NOTE the test FS tracks which kinds of objects are associated
     with which blk_ids, and attempting to open an object at a blk_id
     with the incorrect type will return none; for create, we expect
     that most uses will use a fresh blk_id; if this is not the case
     an exception should be thrown *)

  val create        : blk_id -> t m
  val open_         : blk_id -> t option m

  val replace       : t -> k -> v -> unit m
  val find_opt      : t -> k -> v option m
  val delete        : t -> k -> unit m

  (** Directory handles *)

  val open_dh       : t -> dh
  val readdir       : dh -> (k*v) list m
  (** This is the empty list IFF there are no more entries; otherwise
     a certain small number of entries are returned, which can safely
     be dealt with by non-tail-recursive functions *)
  val close_dh      : dh -> unit

  val readdir_limit : int 
  (** The max number of entries that can be returned by a readdir call *)
end
