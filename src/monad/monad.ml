(** Monad types 

This is halfway between records and using modules/functors. The idea is to pass a value at
initialization time of type ['t monad].
*)

type ('a,'t) m

module type MONAD = sig
  type t
  val return: 'a -> ('a,t)m
  val ( >>= ): ('a,t) m -> ('a -> ('b,t)m) -> ('b,t)m
end

type 't monad = (module MONAD with type t='t)


module Example() = struct

  (* comptime is everything known at compilation time, which likely includes the monad and
     much else *)
  let f (type t) (comptime:t monad) x = 
    let open (val comptime) in
    x >>= fun x -> return (2*x)

  let _ : 't monad -> (int, 't) m -> (int, 't) m = f

end

