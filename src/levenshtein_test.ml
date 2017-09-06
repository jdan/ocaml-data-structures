open Levenshtein;;

let array_of_string s =
  let arr = Array.make (String.length s) 0 in
  Array.mapi (fun i -> fun _ -> s.[i]) arr;;

assert (0 = lev [| 0 |] [| 0 |]);;
assert (0 = lev [| 1 ; 2 ; 3 |] [| 1 ; 2 ; 3 |]);;
assert (1 = lev [| 1 ; 2 ; 3 ; 4 |] [| 1 ; 2 ; 3 |]);;
assert (1 = lev [| 1 ; 2 ; 3 |] [| 1 ; 2 ; 3 ; 4 |]);;
assert (1 = lev [| 1 ; 2 ; 3 |] [| 1 ; 2 ; 4 |]);;
assert (3 = lev (array_of_string "kitten") (array_of_string "sitting"));;
