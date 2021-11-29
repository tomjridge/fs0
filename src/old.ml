
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
