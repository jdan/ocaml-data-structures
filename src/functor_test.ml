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

(* An either module *)
module Either(Config: sig type t end) = struct
  type 'a t = Left of Config.t | Right of 'a
  let fmap f = function
    | Left l -> Left l
    | Right r -> Right (f r)
end

(* Why can't I combine these two lines? *)
module EitherString = Either(struct type t = string end)
module IncrementEither = Increment(EitherString)

let get_age_either = function
  | "Jordan" -> EitherString.Right 25
  | "Steve" -> EitherString.Right 29
  | "Jake" -> EitherString.Right 1
  | _ -> EitherString.Left "User not found :("

let age_next_year_either = IncrementEither.go

let string_of_age_either = function
  | EitherString.Left e -> e
  | EitherString.Right age -> string_of_int age;;

assert(get_age_either "Jordan" |> age_next_year_either |> string_of_age_either = "26");;
assert(get_age_either "Jake" |> age_next_year_either |> string_of_age_either = "2");;
assert(get_age_either "Melissa" |> age_next_year_either |> string_of_age_either = "User not found :(");;
