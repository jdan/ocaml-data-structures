open Trie;;

assert (trie_from_char_list ['a'] 15 = Trie (None, [('a', Trie (Some 15, []))]));;

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

assert (lookup root ['Q'] = None);;
assert (lookup (insert root ['a'] 10) ['a'] = Some 10);;

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

assert (match_prefix age_db "ja" = ["jane"]);;

let jo_matches = match_prefix age_db "jo";;
assert (List.length jo_matches = 2);;
assert (List.mem "joe" jo_matches);;
assert (List.mem "jordan" jo_matches);;
