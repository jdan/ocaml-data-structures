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
    | (k, v as p) :: rest -> if key = k then (Some v, List.append acc rest)
                             else inner rest (p :: acc)

  in inner pairs []

let rec insert trie s value = match trie with Trie (currValue, children) -> (
  match s with
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

let rec find trie s = match trie with Trie (currValue, children) -> (
  match s with
    | [] -> currValue
    | ch :: rest -> (
        match assoc children ch with
          | (None, _) -> None
          | (Some child, _) -> find child rest
    )
)

(* http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

assert (find root ['Q'] = None);;
assert (find (insert root ['a'] 10) ['a'] = Some 10);;

let trie_from_string_list strs =
  let rec inner trie = function
    | [] -> trie
    | (str, value) :: rest -> inner (insert trie (explode str) value) rest in

  inner root strs;;
