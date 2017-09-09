open Binary_heap;;

module IntMinHeap = BinaryHeap (struct
    type t = int
    let compare = compare
    let show = string_of_int
  end)

module IntMaxHeap = BinaryHeap (struct
    type t = int
    let compare a b = -1 * compare a b
    let show = string_of_int
  end)

let () =
  let min_heap = List.fold_left
      IntMinHeap.insert IntMinHeap.empty_tree [6; 1; 4; 10; 7; 3] in
  assert (Some 1 = IntMinHeap.peek min_heap);
  assert (3 = IntMinHeap.height min_heap);

  let max_heap = List.fold_left
      IntMaxHeap.insert IntMaxHeap.empty_tree [6; 1; 4; 10; 7; 3] in
  assert (Some 10 = IntMaxHeap.peek max_heap);
  assert (3 = IntMaxHeap.height max_heap);
