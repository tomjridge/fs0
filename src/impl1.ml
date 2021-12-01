(** First implementation of interfaces in main_intf *)

open Sexplib.Std
open Util[@@warning "-33"]
open Main_intf

module Monad : MONAD = struct

  type 'a m = 'a

  let ( >>= ) a b = b a

  let return x = x
end
open Monad

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

  let sync () = 
    Printf.printf "Blkdev sync called FIXME\n";
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
  include FREELIST with type 'a m = 'a Monad.m and type blk_id = Blkdev.blk_id
  type blk_id = Blkdev.blk_id[@@deriving sexp]
end = Freelist_1

module _ : FREELIST = Freelist


module Runtime_context = struct

  (* These will be filled in later *)
  type file
  type dir

  (* NOTE we don't have file and dir types yet; we fill them in later *)
  type t = { 
    now: unit -> Times.t;
    freelist: Freelist.t; 
    (* blkdev:Blkdev.t;  only 1 blkdev *)
    live_objs:blk_id -> [ `F of file | `D of dir ] option;
    sync_fs: unit -> unit m  (* sync the entire filesystem *)
}

  type ctxt = t

(*
  let the_ctxt = ref None

  let ctxt () : _ t = 
    match !the_ctxt with 
    | None -> failwith __LOC__
    | Some x -> x
*)
end
open Runtime_context


module Int_map_1 = struct

  include Monad

  type blk_id = Blkdev.blk_id
  type ctxt = Runtime_context.t
                  
  module T = struct
    type t = { ctxt:ctxt; blk_id: blk_id; mutable map:int Int_map.t }      
  end
  include T

  let fn blk_id = Printf.sprintf "int_map_%d" blk_id

  module Tmp = struct
    type t = { blk_id: blk_id; map: (int*int) list }[@@deriving sexp]
    let of_t t = { blk_id=t.T.blk_id; map=(t.map |> Int_map.bindings) }
    let to_t ctxt t = { ctxt; T.blk_id=t.blk_id; map=Int_map.of_seq (List.to_seq t.map) }
  end

  let save t = 
    t |> Tmp.of_t |> Tmp.sexp_of_t |> Sexplib.Sexp.save_hum (fn t.blk_id)

  let load ctxt fn = 
    assert(Sys.file_exists fn);
    Sexplib.Sexp.load_sexp fn |> Tmp.t_of_sexp |> Tmp.to_t ctxt
    
  let create ctxt blk_id = 
    let t = {ctxt;blk_id;map=Int_map.empty} in
    save t;
    return t

  let open_ ctxt blk_id = load ctxt (fn blk_id) |> fun x -> return (Some x)

  let root t = t.blk_id
    
  let debug_used_blks t = return [t.blk_id]

  let destroy t = 
    debug_used_blks t >>= fun xs -> 
    Freelist.free_many t.ctxt.freelist xs

  let height t = 
    Int_map.cardinal t.map |> Base.Int.ceil_log2

  let find_opt t k = Int_map.find_opt k t.map |> return

  let replace t k v = t.map <- Int_map.add k v t.map; return ()

  let delete t k = t.map <- Int_map.remove k t.map; return ()

  let sync t = save t |> return

end

module Int_map' : sig
  include INT_MAP with type 'a m = 'a Monad.m and type blk_id = blk_id and type ctxt=ctxt
end = Int_map_1

module Int_map = Int_map_1 (* expose load/save *)


module Blk_map_1 = struct

  include Int_map

  type t = Int_map.t

  let set_blkid t i blk_id = replace t i blk_id

  let get_blkid t i = find_opt t i

end

module Blk_map' : sig
  include BLK_MAP with type 'a m = 'a Monad.m and type blk_id = blk_id and type ctxt=ctxt
end = Blk_map_1

module Blk_map = Blk_map_1  (* expose additional functions *)



module Kind_1 = struct
  include Monad
  type blk_id = Blkdev.blk_id
  type ctxt = Runtime_context.t

  type file_meta = { f_ino:blk_id; f_size:int; f_nlink:int }  
  (* let's associate the inode number with the root blk_id *)

  type dir_meta = { d_ino:blk_id; d_size:int } (* size is the number of entries *)

  (* NOTE patched later *)
  let live_obj_to_meta : ([ `F of 'f | `D of 'd ] -> [`F of file_meta | `D of dir_meta]) ref = ref (fun _x -> failwith "FIXME")

  let _ = live_obj_to_meta

  let stat ctxt blk_id = 
    ctxt.live_objs blk_id |> function 
    | None -> return None
    | Some x -> (!live_obj_to_meta) x |> fun x -> return (Some x)

end

module Kind_2 : KIND with type 'a m = 'a Monad.m and type blk_id = blk_id = Kind_1

module Kind = Kind_1 (* we want to expose the meta types *)


module File_1 = struct

  include Monad
  include Blkdev

  type nonrec ctxt = ctxt

  module T = struct
    type t = { 
      ctxt:ctxt; 
      root:blk_id; 
      mutable sz:int; 
      mutable times:Times.t;
      blk_map:Blk_map.t }
  end
  include T

  module Tmp = struct
    type t = { 
      root:blk_id; 
      sz:int;
      times:Times.t;
      blk_map_root:blk_id }[@@deriving sexp]
    let of_t t = 
      { root=t.T.root; sz=t.sz; times=t.times; blk_map_root=Blk_map.root t.blk_map }
    let to_t ctxt blk_map t = 
      (* let blk_map = Blk_map.load ctxt (Blk_map.fn t.blk_map_root) in *)
      { ctxt; T.root=t.root; sz=t.sz; times=t.times; blk_map }
  end

  let update_mtim t = 
    (t.ctxt.now ()).mtim |> fun mtim -> 
    t.times <- {t.times with mtim};
    ()
    
  let fn blk_id = Printf.sprintf "file_%d" blk_id

  let save t = 
    Blk_map.save t.blk_map;
    t |> Tmp.of_t |> Tmp.sexp_of_t |> Sexplib.Sexp.save_hum (fn t.root)

  let load ctxt fn =
    assert(Sys.file_exists fn);
    Sexplib.Sexp.load_sexp fn |> Tmp.t_of_sexp |> fun tmp -> 
    Blk_map.(load ctxt (fn tmp.blk_map_root)) |> fun blk_map -> 
    Tmp.to_t ctxt blk_map tmp

  let create ctxt blk_id = 
    Freelist.alloc ctxt.freelist >>= fun x -> 
    Blk_map.create ctxt x >>= fun blk_map ->
    let times = ctxt.now () in
    let t = {ctxt;root=blk_id;sz=0;times;blk_map} in
    save t;
    return t

  let open_ ctxt blk_id = load ctxt (fn blk_id) |> fun x -> return (Some x)

  let read_blkid t i = 
    Blk_map.get_blkid t.blk_map i >>= function
    | None -> 
      (* allocate new blk and return *)
      Freelist.alloc t.ctxt.freelist >>= fun blk_id ->
      Blk_map.set_blkid t.blk_map i blk_id >>= fun () ->
      return blk_id
    | Some blk_id -> return blk_id

  let read_blk t i = 
    read_blkid t i >>= fun blk_id -> 
    Blkdev.read blk_id

  let write_blk t i blk =    
    read_blkid t i >>= fun blk_id -> 
    Blkdev.write blk_id blk >>= fun () -> 
    update_mtim t; return ()

  let reveal_blk t i = 
    let blk_i = i / blk_sz in
    read_blkid t blk_i >>= fun blk_id -> 
    Blkdev.read blk_id >>= fun blk -> 
    return (blk_id,blk, i mod blk_sz)
  
  (** NOTE see explanation in pread.txt *)  
  type pread_t = { 
    off0    : int; (* offset within the file *)
    len0    : int; (* initial length of data to read *)
    blk     : int; (* index of the block we are focused on; 0,1,2... *)
    blkoff  : int; (* offset within the block *)
    len_rem : int; (* total amount of data remaining to read from that point *)
    len     : int; (* data to read from this block *)
    dst_off : int; (* position in dst buffer where we place the data we read *)    
  }     

  (* obviously a lot of these calculations could be simplified *)
  let rec calculate_reads ~off0 ~len0 ~dst_off0 = 
    match len0 = 0 with 
    | true -> []
    | false -> 
      let blk = off0 / blk_sz in
      let blkoff = off0 mod blk_sz in
      let len_rem = len0 in
      let len = min len_rem (blk_sz - blkoff) in
      let dst_off = dst_off0 in
      let r = { off0; len0; blk; blkoff; len_rem; len; dst_off } in
      r :: calculate_reads ~off0:(off0+len) ~len0:(len0 - len) ~dst_off0:(dst_off + len)

  let empty_buf = Bigstringaf.create 0

  let pread t ~off:off0 ~len:len0 =
    (* create a bigstring to hold the result; may be over long in case
       there are not enough bytes; perhaps a FIXME? *)
    match off0 >= t.sz with 
    | true -> return empty_buf
    | false -> 
      (* can't read past the end of the file *)
      let len0 = min len0 (t.sz - off0) in
      let buf = Bigstringaf.create len0 in
      (* we separate into two stages; first stage, we calculate where we
         need to read; second stage we perform the actual reads; this
         allows us to take advantage of possible future parallelism *)
      let reads = calculate_reads ~off0 ~len0 ~dst_off0:0 in
      begin 
        reads  |> iter_k (fun ~k reads -> 
          match reads with
          | [] -> return buf
          | r::rest -> 
            read_blk t r.blk >>= fun blk -> 
            Bigstringaf.blit blk ~src_off:r.blkoff buf ~dst_off:r.dst_off ~len:r.len;
            k rest)
      end

  type pwrite_t = {
    off0    : int; (* offset within the file *)
    len0    : int; (* initial length of data to write *)
    bufoff  : int; (* offset within buf *)
    blk     : int; (* index of the block we are focused on; 0,1,2... *)
    blkoff  : int; (* offset within the block *)
    len     : int; (* data to write to this block at blkoff *)
  }

  let rec calculate_writes ~off0 ~len0 ~bufoff0 =
    match len0 = 0 with
    | true -> []
    | false -> 
      let bufoff = bufoff0 in
      let blk = off0 / blk_sz in
      let blkoff = off0 mod blk_sz in
      let len = min len0 (blk_sz - blkoff) in
      let r = { off0; len0; bufoff; blk; blkoff; len } in
      r :: calculate_writes ~off0:(off0+len) ~len0:(len0 - len) ~bufoff0:(bufoff0+len)

  let pwrite dst ~src:buf ~dst_off = 
    let len0 = Bigstringaf.length buf in
    match len0 > 0 with
    | false -> return ()
    | true -> 
      let writes = calculate_writes ~off0:dst_off ~len0 ~bufoff0:0 in
      writes |> iter_k (fun ~k writes -> 
          match writes with
          | [] -> 
            (* remember to update size and time! *)
            dst.sz <- max dst.sz (dst_off + len0);
            update_mtim dst;
            save dst; (* FIXME? *)
            return ()
          | w::rest -> 
            read_blk dst w.blk >>= fun blk -> 
            Bigstringaf.blit buf ~src_off:w.bufoff blk ~dst_off:w.blkoff ~len:w.len;
            write_blk dst w.blk blk >>= fun () -> 
            k rest)    

  let read t ~pos ~len =
    pread t ~off:(!pos) ~len >>= fun buf -> 
    pos:=!pos + Bigstringaf.length buf;
    return buf

  let write t ~pos ~src =
    (* NOTE pwrite will update mtim *)
    pwrite t ~src ~dst_off:(!pos) >>= fun () -> 
    pos:=!pos + Bigstringaf.length src;
    return ()

  let get_sz t = t.sz

  let get_times t = t.times

  let sync t = 
    save t;
    return ()
      
end

module File 
  : FILE with type 'a m='a m and type blk=blk and type blk_id=blk_id and type ctxt=ctxt
  = File_1


module Dir_1 = struct
  type nonrec 'a m = 'a m
  type nonrec blk_id = blk_id
  type nonrec ctxt = ctxt
      
  type did (* dir id *)  = blk_id[@@deriving sexp]
  type fid (* file id *) = blk_id 
  module Map = String_map
  type map = blk_id Map.t

  type k = string
  type v = blk_id

  module T = struct
    type t = { 
      ctxt:ctxt; 
      root:blk_id; 
      mutable parent:did; 
      mutable times:Times.t;
      mutable map:map 
    }
  end
  include T

  module Tmp = struct
    type t = { root:blk_id; parent:did; times:Times.t; map:(string*blk_id)list }[@@deriving sexp]
    let of_t t = { root=t.T.root; parent=t.parent; times=t.times; map=Map.bindings t.map }
    let to_t ctxt t = { T.ctxt=ctxt; root=t.root; parent=t.parent; times=t.times; map=Map.of_seq (List.to_seq t.map) }
  end

  let update_mtim t = 
    (t.ctxt.now ()).mtim |> fun mtim -> 
    t.times <- {t.times with mtim};
    ()

  let fn blk_id = Printf.sprintf "dir_%d" blk_id

  let save t = 
    t |> Tmp.of_t |> Tmp.sexp_of_t |> Sexplib.Sexp.save_hum (fn t.root)

  let load ctxt fn =
    Sexplib.Sexp.load_sexp fn |> Tmp.t_of_sexp |> Tmp.to_t ctxt

  let create ctxt ~root ~parent = 
    let t = { ctxt; root; parent; times=ctxt.now(); map=Map.empty } in
    save t;
    return t      

  let open_ ctxt root = load ctxt (fn root) |> fun x -> return (Some x)

  let sync t = save t; return ()

  let find_opt t k = Map.find_opt k t.map |> return

  let replace t k v = t.map <- Map.add k v t.map; update_mtim t; return () 
      
  let delete t k = t.map <- Map.remove k t.map; update_mtim t; return ()

  let get_times t = t.times

  (* return entries starting at off, unless we are finished, in which
     case return empty list; note that this allows new entries to be
     returned, and entries are returned in lex order *)
  type dh = { t:t; mutable off:string; mutable finished:bool; mutable closed:bool }

  let readdir_limit = 4096 (* for example *)

  let open_dh t = { t; off=""; finished=false; closed=false }
  (* we assume "" is not a valid name in a directory, and that it is
     minimal in the string ordering (it should be!) *)

  let readdir dh =
    assert(not dh.closed);
    match dh.finished with 
    | true -> return []
    | false -> 
      Map.to_seq_from dh.off dh.t.map |> seq_take (readdir_limit+1) |> fun xs -> 
      let xs = List.rev xs in
      (* drop last entry read, but remember to read from that point in future *)
      let xs = 
        match xs with 
        | [] -> dh.finished <- true; xs
        | (x,_)::xs -> dh.off <- x; xs
      in
      return (List.rev xs)

  let close_dh dh = dh.closed <- true; ()
end

module Dir 
  : DIR with type 'a m='a m and type blk_id=blk_id and type ctxt=ctxt
  = Dir_1
