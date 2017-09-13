open Huffman_coding;;

let _ =
  let tbl = frequency "hello, world!" in
  assert (3 = Hashtbl.find tbl 'l');
  assert (2 = Hashtbl.find tbl 'o');
  assert (1 = Hashtbl.find tbl '!');

  let coding = code_tree "My name is Jordan and this text is super compressed" in
  let dictionary = build_dictionary coding in

  (* encode and decode words *)
  assert (decode coding (encode "text" dictionary) = ['t'; 'e'; 'x'; 't']);
  assert (decode coding (encode "Jordan" dictionary) = ['J'; 'o'; 'r'; 'd'; 'a'; 'n']);

  (* encode and decode a word that doesn't appear in the string (but whose letters do) *)
  assert (decode coding (encode "Justin" dictionary) = ['J'; 'u'; 's'; 't'; 'i'; 'n']);

  (* Make sure we're actually compressing! *)
  assert (8 * String.length "mississippi" >
          (encode "mississippi" dictionary |> List.length));
