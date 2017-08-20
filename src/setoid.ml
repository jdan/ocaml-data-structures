(* Setoids! *)

module type Setoid = sig
  type t
  val equals : t -> t -> bool
end
