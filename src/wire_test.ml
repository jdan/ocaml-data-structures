open Wire;;

let () =
  assert (10 = tap [
      ("z", fun wires -> tap wires "a" + tap wires "b") ;
      ("a", fun _ -> 3) ;
      ("b", fun _ -> 7) ;
    ] "z");

  assert (10 = tap [
      ("z", plus "a" "b") ;
      ("a", constant 3) ;
      ("b", constant 7) ;
    ] "z");

  (* Make a half-adder from inputs a and b *)
  let make_half_adder = fun a -> fun b -> [
      ("d", or_ "a" "b") ;
      ("c", and_ "a" "b") ;
      ("e", not_ "c") ;
      ("s", and_ "d" "e") ;

      ("a", constant a) ;
      ("b", constant b)
    ] in

  (* Test the sum and carry bits *)
  assert (1 = tap (make_half_adder 1 1) "c");
  assert (0 = tap (make_half_adder 1 1) "s");

  assert (0 = tap (make_half_adder 1 0) "c");
  assert (1 = tap (make_half_adder 1 0) "s");

  assert (0 = tap (make_half_adder 0 0) "c");
  assert (0 = tap (make_half_adder 0 0) "s");
