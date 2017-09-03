module type Ord = sig
  type t
  val compare : t -> t -> int
  val show : t -> string
end

module type BST = sig
  type t
  type comparable
  val emptyTree : t
  val insert : t -> comparable -> t
  val find : t -> comparable -> bool
  val height : t -> int
  val string_of_tree : t -> string
end

module type UtilsSig = sig
  type t_
  type v
  val is_empty: t_ -> bool
  val left: t_ -> t_
  val right: t_ -> t_
  val value: t_ -> v
  val compare: v -> v -> int
end

module BinarySearchTreeUtils(BST : UtilsSig) = struct
  let rec find node v =
    if BST.is_empty node then false
    else if BST.compare v (BST.value node) = 0 then true
    else if BST.compare v (BST.value node) < 0 then
      find (BST.left node) v
    else
      find (BST.right node) v

  let rec height node =
    if BST.is_empty node then 0
    else
      let left_height = height (BST.left node) in
      let right_height = height (BST.right node) in
      if left_height > right_height then 1 + left_height
      else 1 + right_height
end

module BinarySearchTree(Ord : Ord)
  (* https://realworldocaml.org/v1/en/html/functors.html#destructive-substitution *)
  : (BST with type comparable := Ord.t) =
struct
  type t =
    | Empty
    | Node of { value: Ord.t;
                left: t;
                right: t;
              }

  let emptyTree = Empty

  let rec string_of_tree = function
    | Empty -> "_"
    | Node n ->
      "("
      ^ (Ord.show n.value)
      ^ ", "
      ^ (string_of_tree n.left)
      ^ ", "
      ^ (string_of_tree n.right)
      ^ ")"

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

  include BinarySearchTreeUtils(struct
      type t_ = t   (* ... How do I avoid this? *)
      type v = Ord.t
      let is_empty = (=) Empty
      let left (Node n) = n.left
      let right (Node n) = n.right
      let value (Node n) = n.value
      let compare = Ord.compare
    end)
end
