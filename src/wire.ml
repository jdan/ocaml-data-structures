(* This module emulates "wires" in which we can store procedures and
   populate their arguments later. For example:

     z <- a + b
     a <- 3
     b <- 7
     tap z
       => 10

   Taken from a ruby solution for Advent of Code 2015 (day 7)
      http://adventofcode.com/2015/day/7
      https://github.com/jdan/adventofcode/blob/master/2015/07a-wires.rb (original)
*)

(* List.assoc, but returns a default value of (-> 0) instead of raising *)
let rec assoc wires k = match wires with
  | [] -> (fun _ -> 0)
  | (head, value) :: tail ->
    if head = k
    then value
    else assoc tail k

(* Tap the value of a wire.

   Each wire is defined as a function which accepts all the wires as its first
   argument. This is because wires typically need to `tap` the collection for
   any operation.

   This makes the type definition of `tap` recursive! So we'll need to enable the
   -rectypes flag for merlin and ocamlc.
*)
let tap wires key = (assoc wires key) wires

(* Some utility functions for creating wires *)
let constant k = fun _ -> k
let binary_op op a b = fun wires -> op (tap wires a) (tap wires b)

let plus a b = binary_op (+) a b
let minus a b = binary_op (-) a b
let times a b = binary_op ( * ) a b
let div a b = binary_op (/) a b

(* Some simple binary operations assuming 0 and 1 *)
let and_ a b = binary_op (fun a -> fun b ->
    match (a, b) with
    | (1, 1) -> 1
    | _ -> 0
  ) a b

let or_ a b = binary_op (fun a -> fun b ->
    match (a, b) with
    | (0, 0) -> 0
    | _ -> 1
  ) a b

let not_ a = (fun wires -> match tap wires a with
    | 0 -> 1
    | _ -> 0
  )
