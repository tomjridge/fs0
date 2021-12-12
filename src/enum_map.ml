(*

(** An enum_map (badly named) is a map from an initial segment of the
   natural numbers (0..<N) to ints.

In the context of filesystems, we use an enum map to map the nth block
   of a file to a real block in the underlying filesystem. We handle
   partiality by mapping some ints to some non-wf blk_id. Actually,
   blocks all map to this non-wf blk_id initially.  *)

open Main_intf

open Impl1.Monad

type blk_id (* = int *)
type blk_int = int_bigarray

type ctxt = { 
  alloc       : unit -> blk_id m;
  fill_blk    : int -> blk_int -> unit;

  blk_id_none : blk_id;
  read        : blk_id -> blk_int m;
  write       : blk_id -> blk_int -> unit m
}

type t = {
  ctxt: ctxt;
  blk_id_none: blk_id;
  mutable root: blk_id;
  mutable height: int;
}

let create ~ctxt ~root = {
  ctxt;
  blk_id_none=ctxt.blk_id_none;
  root;
  height=0;
}


let incr_height t = 
  ctxt.alloc () >>= fun blk_id

(** get the nth blk, underlying blk_id; if not already allocated, this
   will force the nth blk to be allocated in the map *)
let get_nth: nth:int -> blk_id = fun ~nth -> 
  
  
*)
