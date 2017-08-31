module type Ord = sig
  type t
  val compare : t -> t -> int
end

module BinarySearchTree(Ord : Ord) = struct
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
