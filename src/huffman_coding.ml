type t =
  | Leaf of {
      weight: int;
      char: char;
    }
  | Node of {
      weight: int;
      left: t;
      right: t;
    }

let weight = function
  | Leaf { weight = w; _ } -> w
  | Node { weight = w; _ } -> w

module NodeMinHeap = Binary_heap.BinaryHeap (struct
    type nonrec t = t
    let compare a b = compare (weight a) (weight b)

    (* It's pretty silly that Ord requires a `show` method, but useful
       for debugging I guess *)
    let rec show = function
      | Leaf l ->
        "[" ^ (string_of_int l.weight) ^ " " ^ (String.make 1 l.char) ^ "]"
      | Node n ->
        "[" ^ (string_of_int n.weight) ^ " " ^
        "(" ^ (show n.left) ^ ", " ^ (show n.right) ^ ")]"
  end)

(* TODO: frequency analysis based on a block size *)
let frequency str =
  let tbl = Hashtbl.create 40 in
  let char_list = Trie.explode str in
  List.iter
    (fun ch ->
       if Hashtbl.mem tbl ch then
         Hashtbl.replace tbl ch (1 + Hashtbl.find tbl ch)
       else
         Hashtbl.add tbl ch 1)
    char_list;
  (* Return the table *)
  tbl

let code_tree str =
  let extract_two heap =
    let (a, heap_once) = NodeMinHeap.extract heap in
    let (b, heap_twice) = NodeMinHeap.extract heap_once in
    match (a, b) with
    | (Some a_, Some b_) -> (a_, b_, heap_twice)
    | _ -> raise (Invalid_argument "Cannot extract twice from this heap") in

  (* Turn frequency analysis key/values into leaves and insert them into
     a min-heap *)
  let create_min_heap tbl =
    Hashtbl.fold
      (fun k -> fun v -> fun heap ->
         Leaf { char = k;
                weight = v;
              }
         |> NodeMinHeap.insert heap)
      tbl
      NodeMinHeap.empty_tree in

  (* Reduce the tree by extracting the two smallest items, combining
     them, and re-inserting into the min-heap *)
  let rec reduce_min_heap heap =
    if NodeMinHeap.height heap = 1 then heap
    else
      let (a, b, new_heap) = extract_two heap in
      Node {
        weight = weight a + weight b;
        left = a;
        right = b;
      }
      |> NodeMinHeap.insert new_heap
      |> reduce_min_heap in

  frequency str
  |> create_min_heap
  |> reduce_min_heap
  |> NodeMinHeap.peek
  |> function
  | Some coding -> coding
  | None -> raise (Invalid_argument "failed to code string")

let build_dictionary coding =
  let rec inner node coding_thus_far = match node with
    | Leaf { char = ch; _ } ->
      (* reverse `coding_thus_far` because we append bits to the front
         while recursing *)
      [(ch, List.rev coding_thus_far)]
    | Node { left = l; right = r; _ } ->
      List.append
        (inner l (0 :: coding_thus_far))
        (inner r (1 :: coding_thus_far)) in
  inner coding []

let encode str dictionary =
  Trie.explode str |>
  List.fold_left
    (fun acc -> fun ch ->
       List.assoc ch dictionary |>
       List.append acc)
    []

let decode tree stream =
  let rec inner node stream = match (node, stream) with
    (* Check this case first so we don't accidentally forget our
       character when our stream is empty! *)
    | (Leaf { char = c; _ }, []) -> [c]

    (* Now check base cases (empty stream, leaf) *)
    | (_, []) -> []
    | (Leaf { char = c; _ }, _) -> c :: inner tree stream

    (* Recursive step, pop the stream and traverse our coding tree
       accordingly *)
    | (Node { left = l; right = r; _ }, hd :: tail) ->
      if hd = 0 then inner l tail
      else if hd = 1 then inner r tail
      else raise (Invalid_argument
                    ("unable to parse stream (bit: " ^ (string_of_int hd) ^ ")"))
  in inner tree stream
