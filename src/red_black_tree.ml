open Binary_search_tree;;

module RedBlackTree(Ord : Ord)
  : (BST with type comparable := Ord.t) =
struct
  type color = Red | Black
  type t =
    | Empty
    | Node of { mutable color : color;
                value: Ord.t;
                mutable parent : t;
                mutable left : t;
                mutable right : t;
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

  let get_parent = function
    (* The parent of an empty node is...empty? Could be None *)
    | Empty -> Empty
    | Node n -> n.parent

  let get_grand_parent = function
    | Empty -> Empty
    | Node n -> get_parent n.parent

  let get_uncle = function
    | Empty -> Empty
    | Node n -> (match get_grand_parent (Node n) with
        | Empty -> Empty
        | Node gp ->
          if gp.left == n.parent then gp.right
          else gp.left
      )

  let rotate_left (Node n) =
    let (Node right) = n.right in
    (* Swap parents *)
    right.parent <- n.parent;
    n.parent <- (Node right);

    (* Swap some children *)
    n.right <- right.left;
    right.left <- (Node n);

    (* Finally, adjust our old parent's child *)
    match right.parent with
    | Empty -> ()
    | Node p ->
      if (Node n) == p.left
      then p.left <- (Node right)
      else p.right <- (Node right)

  let rotate_right (Node n) =
    let (Node left) = n.left in
    (* Swap parents *)
    left.parent <- n.parent;
    n.parent <- (Node left);

    (* Swap some children *)
    n.left <- left.right;
    left.right <- (Node n);

    (* Finally, adjust our old parent's child *)
    match left.parent with
    | Empty -> ()
    | Node p ->
      if (Node n) == p.left
      then p.left <- (Node left)
      else p.right <- (Node left)

  let rebalance node =
    (* We're following the cases as defined in:
       https://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Insertion *)
    let rec case_1 (Node n) =
      if n.parent = Empty then
        n.color <- Black
      else
        case_2 (Node n)

    and case_2 (Node n) =
      let (Node p) = n.parent in
      if p.color = Black then ()
      else
        case_3 (Node n)

    and case_3 (Node n) =
      let u = get_uncle (Node n) in match u with
      | Empty -> case_4 (Node n)
      | (Node u) ->
        if u.color != Red then case_4 (Node n)
        else
          let (Node p) = n.parent in
          let (Node gp) = get_grand_parent (Node n) in
          p.color <- Black;
          u.color <- Black;
          gp.color <- Red;
          case_1 (Node gp)

    and case_4 (Node n) =
      let (Node p) = n.parent in
      let (Node gp) = get_grand_parent (Node n) in

      if ((Node n) == p.right && (Node p) == gp.left) then
        begin
          rotate_left (Node p);
          case_5 n.left
        end
      else if (Node n) == p.left && (Node p) == gp.right then
        begin
          rotate_right (Node p);
          case_5 n.right
        end
      else
        case_5 (Node n)

    and case_5 (Node n) =
      let (Node p) = n.parent in
      let (Node gp) = get_grand_parent (Node n) in
      p.color <- Black;
      gp.color <- Red;

      if (Node n) == p.left
      then rotate_right (Node gp)
      else rotate_left (Node gp)

    in case_1 node; node

  let rec root_of_node node = match get_parent node with
    | Empty -> node
    | Node p -> root_of_node (Node p)

  let insert tree value =
    let rec naive_insert tree parent = match tree with
      (* We'll return the node instead of the tree, since we'll
         balance the tree based on the node's ancestry *)
      | Empty ->
        let node =
          Node { color = Red;
                 value = value;
                 parent = parent;
                 left = Empty;
                 right = Empty;
               }
        in (node, node)

      | Node n ->
        if Ord.compare value n.value < 0
        then
          let (node, whole_tree) = naive_insert n.left tree in
          (node, (
              n.left <- whole_tree;
              Node n
            ))
        else
          let (node, whole_tree) = naive_insert n.right tree in
          (node, (
              n.right <- whole_tree;
              Node n
            )) in

    get_parent tree
    |> naive_insert tree
    |> (fun (node, _) -> node)
    |> rebalance
    |> root_of_node

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
