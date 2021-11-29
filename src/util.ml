(** Essentially the Y combinator; useful for anonymous recursive
   functions. The k argument is the recursive callExample:

{[
  iter_k (fun ~k n -> 
      if n = 0 then 1 else n * k (n-1))

]}


 *)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x

let dest_Some = function
  | None -> failwith "dest_Some"
  | Some x -> x

let warn (s:unit->string) = 
  print_endline (s())

let debug (s:unit->string) = 
  print_endline (s())


(* t1 and t2 are ctypes kinds; t2_kind is a normal bigarray kind *)
let coerce_bigarray1 t1 t2 t2_kind arr = 
  Ctypes.bigarray_start Ctypes.array1 arr |> fun (pi:'t1 Ctypes.ptr) -> 
  Ctypes.(coerce (ptr t1) (ptr t2) pi) |> fun (pc:'t2 Ctypes.ptr) -> 
  Ctypes.bigarray_of_ptr (* this function forces C layout *) 
    Ctypes.array1 
    ((Bigarray.Array1.dim arr * Ctypes.(sizeof t1)) / Ctypes.(sizeof t2))
    t2_kind
    pc |> fun arr -> 
  arr

let coerce_ba_c2i arr_c = 
  let arr_i = coerce_bigarray1 Ctypes.char Ctypes.camlint Bigarray.Int arr_c in
  arr_i


let coerce_ba_i2c arr_i =
  let arr_c = coerce_bigarray1 Ctypes.camlint Ctypes.char Bigarray.Char arr_i in
  arr_c

