(* Stack! *)

type 'a t =
  | Empty
  | Item of 'a * 'a t

let rec push stack item = match stack with
  | Empty -> Item (item, Empty)
  | tail -> Item (item, tail)

let pop = function
  | Empty -> None
  | Item (head, rest) -> Some (head, rest)

let peek = function
  | Empty -> None
  | Item (head, _) -> Some head

let rec stack_of_list = function
  | [] -> Empty
  | head :: tail -> Item (head, stack_of_list tail)

let rec list_of_stack = function
  | Empty -> []
  | Item (head, rest) -> head :: list_of_stack rest
