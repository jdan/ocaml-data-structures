module type S = sig
  type t
  val compare : t -> t -> int
  val show : t -> string
end
