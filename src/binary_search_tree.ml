module type Ord = sig
  type t
  val compare : t -> t -> int
  val show : t -> string
end

module type BSTPassThruFields = sig
  type t
  type comparable
  val empty_tree : t
  val insert : t -> comparable -> t
end

module type InputBST = sig
  include BSTPassThruFields

  val left: t -> t
  val right: t -> t
  val value: t -> comparable
end

module type BST = sig
  include BSTPassThruFields

  val find : t -> comparable -> bool
  val height : t -> int
  val string_of_tree : t -> string
end

module Make
    (Ord: Ord)
    (Base: InputBST with type comparable := Ord.t)
  : (BST with type comparable := Ord.t) =
struct
  include Base

  let rec find node v =
    if node = Base.empty_tree then false
    else if Ord.compare v (Base.value node) = 0 then true
    else if Ord.compare v (Base.value node) < 0 then
      find (Base.left node) v
    else
      find (Base.right node) v

  let rec height node =
    if node = Base.empty_tree then 0
    else
      let left_height = height (Base.left node) in
      let right_height = height (Base.right node) in
      if left_height > right_height then 1 + left_height
      else 1 + right_height

  let rec string_of_tree node =
    if node = Base.empty_tree then "_"
    else
      "("
      ^ (Ord.show @@ value node)
      ^ ", "
      ^ (string_of_tree @@ left node)
      ^ ", "
      ^ (string_of_tree @@ right node)
      ^ ")"
end

module BinarySearchTree
    (Ord : Ord)
  : (BST with type comparable := Ord.t) =
  Make (Ord) (struct
    type t =
      | Empty
      | Node of { value: Ord.t;
                  left: t;
                  right: t;
                }

    let empty_tree = Empty
    let left (Node n) = n.left
    let right (Node n) = n.right
    let value (Node n) = n.value

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
  end)
