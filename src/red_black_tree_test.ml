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

let rec range a b =
  if a = b then [b]
  else a :: range (a + 1) b

let () =
  let int_rb_tree = List.fold_left IntRBT.insert IntRBT.empty_tree [
      1; 2; 3; 4; 5; 6;
    ] in

  assert (IntRBT.find int_rb_tree 3);
  assert (IntRBT.find int_rb_tree 6);
  assert (not (IntRBT.find int_rb_tree 7));

  assert (
    "(2, (1, _, _), (4, (3, _, _), (5, _, (6, _, _))))" =
    IntRBT.string_of_tree int_rb_tree
  );

  let person_rb_tree = List.fold_left PersonRBT.insert PersonRBT.empty_tree [
      ("Jake", 1) ;
      ("Victoria", 4) ;
      ("Jordan", 25) ;
      ("Steve", 29) ;
      ("Rob", 33) ;
    ] in

  assert (PersonRBT.find person_rb_tree ("Steve", 29));
  assert (PersonRBT.find person_rb_tree ("Jordan", 25));
  assert (not (PersonRBT.find person_rb_tree ("Jordan", 24)));
  assert (
    "((Victoria, 4), ((Jake, 1), _, _), ((Steve, 29), " ^
    "((Jordan, 25), _, _), ((Rob, 33), _, _)))" =
    PersonRBT.string_of_tree person_rb_tree
  );

  (* Assert that balancing works for left-heavy and right-heavy trees *)
  assert (
    (=)
      (List.fold_left IntRBT.insert IntRBT.empty_tree (range 1 10000)
       |> IntRBT.height)
      (List.fold_left IntRBT.insert IntRBT.empty_tree (range 1 10000 |> List.rev)
       |> IntRBT.height)
  )
