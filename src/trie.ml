(* Trie! *)

(* NOTE: For now we'll encode strings to 'a, but a good thought exercise
   would be to paramterize the type of thing we want to encode *)
type 'a t = Trie of ('a option) * ((char * 'a t) list)

let root = Trie (None, [])

let rec trie_from_char_list s value = match s with
  | [] -> Trie (Some value, [])
  | ch :: rest -> Trie (None, [(ch, trie_from_char_list rest value)])

(* assoc, but we return the other stuff too *)
let assoc pairs key =
  let rec inner pairs acc = match pairs with
    | [] -> (None, acc)
    | (k, v) :: rest when k = key -> (Some v, List.append acc rest)
    | head :: rest -> inner rest (head :: acc)

  in inner pairs []

let rec insert (Trie (currValue, children)) s value = match s with
  | [] -> Trie (Some value, children)   (* assign our value to the node *)
  | ch :: rest -> (
      match assoc children ch with
      | (None, _) ->
        (* Prepend a new trie from our string to the children *)
        Trie (currValue, (ch, trie_from_char_list rest value) :: children)
      | (Some child, otherNodes) ->
        (* Pass the rest of the string to the child, building a new trie with
            it and the node's other children *)
        Trie (currValue, (ch, insert child rest value) :: otherNodes)
    )

let rec traverse (Trie (currValue, children) as trie) = function
  | [] -> Some trie
  | ch :: rest -> (
      match assoc children ch with
      | (None, _) -> None
      | (Some child, _) -> traverse child rest
    )

let rec lookup trie s = match traverse trie s with
  | None -> None
  | Some (Trie (value, _)) -> value

(* http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* walk a trie and return values *)
let walk trie =
  let rec inner (Trie (currentValue, children)) str_so_far =
    let child_values = List.flatten (
        List.map
          (fun (ch, child) -> inner child (str_so_far ^ Char.escaped ch))
          children)

    in match currentValue with
    | None -> child_values
    | Some value -> str_so_far :: child_values

  in inner trie ""

let trie_from_string_list strs =
  let rec inner trie = function
    | [] -> trie
    | (str, value) :: rest -> inner (insert trie (explode str) value) rest in

  inner root strs

let match_prefix trie str = match (traverse trie (explode str)) with
  | None -> []
  | Some subtrie -> List.map (fun (item) -> str ^ item) (walk subtrie)
