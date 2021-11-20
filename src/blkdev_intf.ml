type int_bigarray = (int,Bigarray.int_elt,Bigarray.c_layout)Bigarray.Array1.t
type bigarray_int = int_bigarray
type bigstring = Bigstringaf.t

module type MONAD = sig
  type 'a m
end

module type BLKDEV = sig
  
  type blk = bigstring
  type blk_id (* = int, with 0 representing "None" *)

  val blk_sz: int (* as char *)


  type blk_int = bigarray_int
  val to_blk_int : blk -> blk_int
  val blk_sz_int: int  (* blk_int size *)
  val to_blk : blk_int -> blk

  type 'a m
  val write: blk_id -> blk -> unit m
  val read : blk_id -> blk m  
    
end


(** Persistent doubly-linked list. FIXME how do we load? traverse
   entire list at initialization time? *)
module type DLINKED_LIST = sig

  type elt
  type blk_id 
    
  type t (* = { hd : blk_id; tl:blk_id; } *)
  val hd          : t -> blk_id

  type 'a m
  val used_blks : t -> blk_id list m
  val add       : t -> elt -> unit m
  val add_many  : t -> elt list -> unit m
  val to_list   : t -> elt list m
      
end


module type DLINKED_LIST_INT = sig

  include DLINKED_LIST with type elt = int

end


module type FREELIST = sig

  type elt 
  type blk_id

  type freelist (* = { hd:blk_id } *)
  val hd          : freelist -> blk_id

  type 'a m

  val used_blks   : freelist -> blk_id list m
  val alloc       : unit -> blk_id m
  val alloc_clean : unit -> blk_id m
  val free        : blk_id -> unit
  val elts        : freelist -> elt list m
  val alloc_many  : int -> blk_id list m
  val free_many   : blk_id list -> unit m  

end

(* Root blk *)
module type BLK0 = sig
  type blk_id = int
  type t = { freelist_root: blk_id; prefiles: blk_id array; (* ... *) }
  type 'a m

  val read: unit -> t m
  val write: t -> unit m
  
end


(** A map from blk index to blk_id, maintained on disk and mirrored in
   memory *)
module type BLK_MAP = sig

  type t (* = { root: blk_id } *)
  type blk_id
  type 'a m

  val height: t -> int  (** height of the underlying tree on disk *)

  val set_blkid: int -> blk_id -> unit
  (** associate blk_id with a given blk index in the file *)

  val get_blkid: int -> blk_id option
  (** given a blk index, return the corresponding blk_id *)

  val sync_map : unit -> unit m
end

(** A prefile maintains a map from blk index to blk_id, in memory (for
   quick lookup) *)
module type PREFILE = sig

  type t (* = { blk_map:Blk_map.t; sz:int }; on disk we have a blk
            that has a pointer to the blk_map, a sz field, and
            possibly some other metadata *)
  type blk
  type blk_id
  type 'a m

  val read_blk : int -> blk m
  val write_blk: int -> blk -> unit m
  (** This will allocate a new blk_id if needed *)

  val used_blks: t -> blk_id list m

  (* val sz:int FIXME not sure about this; perhaps just have a blk_map
     *)
  (** The size is the size in bytes; this is to facilitate the
     implementation of a real file on top of this interface *)
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
