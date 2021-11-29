(** First implementation of interfaces in main_intf *)

open Sexplib.Std
open Util[@@warning "-33"]
open Main_intf

module Monad : MONAD = struct

  type 'a m = 'a

  let ( >>= ) a b = b a

  let return x = x

end

module Blkdev_1 = struct

  include Monad

  type blk = bigstring

  type blk_id = int[@@deriving sexp]

  let blk_id_none = -1

  let blk_id_is_some x = x>=0

  let blk_sz = 4096

  type blk_int = bigarray_int

  let to_blk_int (blk:blk) : blk_int = Util.coerce_ba_c2i blk
      
  (* check we are on 64bit arch *)
  let _ = assert(Sys.int_size = 8)

  let blk_sz_int = blk_sz / 8 

  let to_blk (blk:blk_int) : blk = Util.coerce_ba_i2c blk

  let blks = Hashtbl.create 100

  let read blk_id = 
    (match Hashtbl.find_opt blks blk_id with
     | None -> 
       (Bigstringaf.create blk_sz) |> fun blk -> 
       Hashtbl.replace blks blk_id blk;
       blk
     | Some blk -> blk)
    |> return

  (* NOTE we currently enforce that you can't write blk to a blk_id
     different from that when it was created *)
  let write blk_id blk =
    assert(Hashtbl.find_opt blks blk_id = Some blk);
    return ()

end

module Blkdev : sig
  include BLKDEV with type blk_id = int and type 'a m = 'a Monad.m
  type blk_id = int[@@deriving sexp]
end = Blkdev_1

open Blkdev

module _ : BLKDEV = Blkdev

module Freelist_1 = struct

  include Monad

  type blk_id = Blkdev.blk_id[@@deriving sexp]

  type t = { 
    blk_id:blk_id; 
    fn: string; 
    mutable max_used: int; 
    mutable frees:blk_id list }[@@deriving sexp]

  let fn i = Printf.sprintf "freelist_%d" i

  let save t = Sexplib.Sexp.save_hum t.fn (t |> sexp_of_t)

  let load fn = Sexplib.Sexp.load_sexp fn |> t_of_sexp

  let create blk_id = 
    { blk_id; fn=fn blk_id; max_used=0; frees=[] } |> fun t -> 
    save t;
    return t

  let open_ blk_id = 
    assert(Sys.file_exists (fn blk_id));
    load (fn blk_id) |> fun x -> return (Some x)

  let alloc' t = 
    match t.frees with
    | [] -> 
      t.max_used <- t.max_used + 1; 
      save t;
      return t.max_used
    | x::xs -> 
      t.frees <- xs; 
      save t;
      return x

  let alloc ?clean t = 
    alloc' t >>= fun blk_id -> 
    let blk = Bigstringaf.create blk_sz in
    let blk_int = to_blk_int blk in
    begin
      match clean with
      | None -> ()
      | Some (`Char c) -> 
        for i = 0 to blk_sz do blk.{ i } <- c done
      | Some (`Int j) ->
        for i = 0 to blk_sz do blk_int.{ i } <- j done
    end;
    Blkdev.write blk_id blk >>= fun () -> 
    return blk_id

  let free t blk_id = 
    t.frees <- blk_id::t.frees;
    save t;
    return ()

  let alloc_many ?clean t i =
    (0,[]) |> iter_k (fun ~k (j,ids) -> 
        match j >= i with 
        | true -> return ids
        | false -> 
          alloc ?clean t >>= fun id -> 
          k (j+1,id::ids))

  let rec free_many t ids =
    match ids with 
    | [] -> return ()
    | x::xs -> free t x >>= fun () -> free_many t xs

  let debug_used_blks t = return [t.blk_id]
  

end

module Freelist : sig
  include FREELIST with type blk_id = Blkdev.blk_id
  type blk_id = Blkdev.blk_id[@@deriving sexp]
end = Freelist_1

module _ : FREELIST = Freelist

