open Red_black_tree;;

module Person = struct
  type t = string * int
  let compare (_, a) (_, b) = compare a b
  let show (name, age) =
    "(" ^ name ^ ", " ^ (string_of_int age) ^ ")"
end

module Int = struct
  type t = int
  let compare = compare
  let show = string_of_int
end

module PersonRBT = RedBlackTree(Person)
module IntRBT = RedBlackTree(Int)

let () =
  assert(
    "(2, (1, _, _), (4, (3, _, _), (5, _, (6, _, _))))" =
    begin
      List.fold_left IntRBT.insert IntRBT.emptyTree [
        1; 2; 3; 4; 5; 6;
      ] |> IntRBT.string_of_tree
    end);

  assert(
    "((Victoria, 4), ((Jake, 1), _, _), ((Steve, 29), " ^
    "((Jordan, 25), _, _), ((Rob, 33), _, _)))" =
    begin
      List.fold_left PersonRBT.insert PersonRBT.emptyTree [
        ("Jake", 1) ;
        ("Victoria", 4) ;
        ("Jordan", 25) ;
        ("Steve", 29) ;
        ("Rob", 33) ;
      ] |> PersonRBT.string_of_tree
    end
  )
