(* Functors! *)

(* Not OCaml functors, which map modules to modules, but
   Haskell-type Functors that implement an `fmap` *)
module type Functor = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
