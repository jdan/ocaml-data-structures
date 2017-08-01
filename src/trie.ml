(* Trie! *)

(* NOTE: For now we'll encode strings to 'a, but a good thought exercise
   would be to paramterize the type of thing we want to encode *)
type 'a t = Trie of ('a option) * ((char * 'a t) list)

let root = Trie (None, [])

let rec trie_from_char_list s value = match s with
  | [] -> Trie (Some value, [])
  | ch :: rest -> Trie (None, [(ch, trie_from_char_list rest value)]);;

assert (trie_from_char_list ['a'] 15 = Trie (None, [('a', Trie (Some 15, []))]));;

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
      );;

assert (trie_from_char_list ['a'] 15 = insert root ['a'] 15);;

assert (insert (insert root ['a'] 10) ['a'; 't'] 20 =
          Trie (None, [
            ('a', Trie (Some 10, [
              ('t', Trie (Some 20, []))
            ]))
          ]));;

(* Double inserts are a-okay *)
assert (insert (insert root ['a'] 15) ['a'] 77 =
          Trie (None, [('a', Trie (Some 77, []))]));;

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
  exp (String.length s - 1) [];;

assert (lookup root ['Q'] = None);;
assert (lookup (insert root ['a'] 10) ['a'] = Some 10);;

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

  inner root strs;;

let age_db = trie_from_string_list [("jordan", 25); ("joe", 13); ("jane", 20)];;
(* lookup tests *)
assert (lookup age_db (explode "jordan") = Some 25);;
assert (lookup age_db (explode "jane") = Some 20);;
assert (lookup age_db (explode "jackson") = None);;

(* walk tests *)
assert (List.length (walk age_db) = 3);;
assert (List.mem "jordan" (walk age_db));;
assert (List.mem "jane" (walk age_db));;
assert (List.mem "joe" (walk age_db));;

let match_prefix trie str = match (traverse trie (explode str)) with
  | None -> []
  | Some subtrie -> List.map (fun (item) -> str ^ item) (walk subtrie);;

assert (match_prefix age_db "ja" = ["jane"]);;

let jo_matches = match_prefix age_db "jo";;
assert (List.length jo_matches = 2);;
assert (List.mem "joe" jo_matches);;
assert (List.mem "jordan" jo_matches);;
