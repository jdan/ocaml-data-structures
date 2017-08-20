open Functor;;

(* Let's define an OCaml functor that `fmap`s n -> n + 1 *)
module Increment(F : Functor) = struct
  let go = F.fmap ((+) 1)
end

(* Define a Maybe functor *)
module Maybe = struct
  type 'a t = 'a option
  let fmap f x = match x with
    | Some v -> Some (f v)
    | None -> None
end

(* Let's increment some Maybes *)
module IncrementMaybe = Increment(Maybe)

let get_age = function
  | "Jordan" -> Some 25
  | "Steve" -> Some 29
  | "Jake" -> Some 1
  | _ -> None

let age_next_year = IncrementMaybe.go

let string_of_age = function
  | None -> "User not found"
  | Some age -> string_of_int age;;

assert(get_age "Jordan" |> age_next_year |> string_of_age = "26");;
assert(get_age "Jake" |> age_next_year |> string_of_age = "2");;
assert(get_age "Melissa" |> age_next_year |> string_of_age = "User not found");;

(* Let's increment some Stacks! Which are also Functors *)
module IncrementStack = Increment(Mystack)
let stack = Mystack.stack_of_list [3 ; 5 ; 1 ; 2];;
assert (IncrementStack.go stack |> Mystack.list_of_stack = [4 ; 6 ; 2; 3]);;
