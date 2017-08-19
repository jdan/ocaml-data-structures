(* Setoids! *)

module type Setoid = sig
  type t
  val equals : t -> t -> bool
end

(* A simple Person module *)
module Person = struct
  type t = string * int

  let make name age: t = (name, age)
  let name (n, _) = n
  let age (_, a) = a

  (* Not ready for production ;) *)
  let equals p1 p2 = (name p1 = name p2) && (age p1 = age p2)
end

(* We can uniquify any List of setoids - all we need is an `equals` *)
module Uniq = functor(Setoid : Setoid) -> struct
  (* Like List.mem, but using Setoid.equals *)
  let rec member needle = function
    | [] -> false
    | head :: tail ->
      if Setoid.equals needle head then true
      else member needle tail

  (* Our main function which uniquifies a list of setoids *)
  let rec f = function
    | [] -> []
    | hd :: [] -> [hd]
    | hd :: tail ->
      if member hd tail then f tail
      else hd :: f tail
end

(* Use our Uniq functor on our Person module *)
module PersonUniq = Uniq(Person)
