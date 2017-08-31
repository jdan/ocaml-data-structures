open Binary_search_tree;;

module IntBST = BinarySearchTree(
  struct
    type t = int
    let compare = compare
  end)

let tree = List.fold_left IntBST.insert IntBST.emptyTree [5; 6; 7; 1; 10; 3];;
assert (IntBST.find tree 5);;
assert (IntBST.find tree 6);;
assert (IntBST.find tree 3);;
assert (not @@ IntBST.find tree 2);;
