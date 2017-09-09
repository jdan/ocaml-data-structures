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

let rec extract_acc extractor heap =
  match extractor heap with
  | (None, _) -> []
  | (Some v, new_heap) -> v :: extract_acc extractor new_heap

let () =
  let min_heap = List.fold_left
      IntMinHeap.insert IntMinHeap.empty_tree [6; 1; 4; 10; 7; 3] in
  (* Test idemopotency *)
  let _ = IntMinHeap.insert min_heap 0 in

  assert (Some 1 = IntMinHeap.peek min_heap);
  assert (3 = IntMinHeap.height min_heap);
  assert ([1; 3; 4; 6; 7; 10] = extract_acc IntMinHeap.extract min_heap);

  let max_heap = List.fold_left
      IntMaxHeap.insert IntMaxHeap.empty_tree [6; 1; 4; 10; 7; 3] in
  (* Test idemopotency *)
  let _ = IntMaxHeap.insert max_heap 100 in

  assert (Some 10 = IntMaxHeap.peek max_heap);
  assert (3 = IntMaxHeap.height max_heap);
  assert ([10; 7; 6; 4; 3; 1] = extract_acc IntMaxHeap.extract max_heap);
