open Binary_search_tree;;

module IntBST = BinarySearchTree(struct
    type t = int

    (* OCaml provides a top-level `compare` method which
       works on a few types (int included) *)
    let compare = compare
    let show = string_of_int
  end)

let tree = List.fold_left IntBST.insert IntBST.empty_tree [5; 6; 7; 1; 10; 3];;
assert (IntBST.find tree 5);;
assert (IntBST.find tree 6);;
assert (IntBST.find tree 3);;
assert (not @@ IntBST.find tree 2);;
assert (4 = IntBST.height tree)
