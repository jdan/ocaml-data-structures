module type S = sig
  include Binary_search_tree.BST
  val peek : t -> comparable option
end

module BinaryHeap (Ord : Ord.S)
  : (S with type comparable := Ord.t) =
struct
  type heap = { size: int;
                items: Ord.t option array;
              }

  (* Our type t is a heap with an index *)
  type t = { heap: heap;
             index: int;
           }

  (* Heaps aren't Binary search trees, so `find` will be useless,
     but we'll get `height` and `string_of_tree` for free.

     TODO: Make another Binary_tree.Make maybe?
  *)
  include (
    Binary_search_tree.Make (Ord) (struct
      type nonrec t = t

      let empty_tree = {
        heap = { items = [| None |];
                 size = 0;
               };
        index = 0;
      }

      let parent_index n = n / 2
      let left_child_index n = 2 * n + 1
      let right_child_index n = 2 * n + 2

      (* Update a node with an index, but return empty_tree
         if that index is out of bounds.

         This allows us to safely call `value` of a node's left
         or right child.
      *)
      let maybe_emptify node index =
        if index >= node.heap.size
        then empty_tree
        else { node with index = index }

      let left node = maybe_emptify node (left_child_index node.index)
      let right node = maybe_emptify node (right_child_index node.index)
      let value node = match node.heap.items.(node.index) with
        | Some v -> v
        | None -> raise (Invalid_argument "index out of bounds")

      let rec bubble_up index root =
        let items = root.heap.items in
        match (items.(index), items.(parent_index index)) with
        | (Some value, Some parent_value) ->
          if Ord.compare value parent_value < 0
          then
            (* Swap em and continue *)
            let tmp = items.(parent_index index) in
            items.(parent_index index) <- items.(index);
            items.(index) <- tmp;
            bubble_up (parent_index index) root
          else
            root

        | _ -> raise (Invalid_argument "index out of bounds")

      let maybe_increase_capacity root =
        if root.heap.size = Array.length root.heap.items then
          let double_capacity_items =
            Array.append
              root.heap.items
              (Array.make root.heap.size None) in

          { root with
            heap = { root.heap with
                     items = double_capacity_items
                   }
          }
        else root

      let insert root value =
        { root with
          heap = let {size; items} = root.heap in
            { items = (
                  items.(size) <- Some value; items
                );
              size = root.heap.size + 1;
            }
        }
        |> bubble_up root.heap.size
        |> maybe_increase_capacity
    end
    ) : (Binary_search_tree.BST
         with type t := t
          and type comparable := Ord.t))

  let peek root = root.heap.items.(0)
end
