module type Ord = sig
  type t
  val compare : t -> t -> int
end

module type BST = sig
  type t
  type comparable
  val emptyTree : t
  val insert : t -> comparable -> t
  val find : t -> comparable -> bool
end

module BinarySearchTree(Ord : Ord)
  (* https://realworldocaml.org/v1/en/html/functors.html#destructive-substitution *)
  : (BST with type comparable := Ord.t) =
struct
  type 'a node =
    { value: Ord.t;
      left: 'a;
      right: 'a;
    }

  type t =
    | Empty
    | Node of t node

  let emptyTree = Empty

  let rec insert node value = match node with
    | Empty -> Node { value = value;
                      left = Empty;
                      right = Empty;
                    }
    | Node n ->
      if Ord.compare value n.value < 0
      then
        Node { n with
               left = insert n.left value }
      else
        Node { n with
               right = insert n.right value }

  let rec find node value = match node with
    | Empty -> false
    | Node n ->
      if Ord.compare value n.value = 0 then
        true
      else if Ord.compare value n.value < 0 then
        find n.left value
      else
        find n.right value
end
