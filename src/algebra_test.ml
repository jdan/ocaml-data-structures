open Algebra;;

(* string_of_expression tests *)
assert ("(x + 1)" = string_of_expression (Addition (Variable "x", Value 1)));;
assert ("(x + (y * 4))" = string_of_expression
          (Addition (Variable "x",
                     Multiplication (Variable "y", Value 4))));;

(* simplify tests *)
assert (Addition (Variable "x", Value 5) = begin
    simplify (Addition (Variable "x", Addition (Value 2, Value 3)))
  end);;

assert (Variable "x" = begin
    simplify (Multiplication (Variable "x", Addition (Value 6, Value (-5))))
  end);;

(* deriv + simplify tests *)
(* d/dx x^2 = 2x *)
assert (Multiplication (Value 2, Variable "x") = begin
    deriv "x" (Exponentiation (Variable "x", 2)) |> simplify
  end);;

(* d/dy x^2 = 0 *)
assert (Value 0 = begin
    deriv "y" (Exponentiation (Variable "x", 2)) |> simplify
  end);;

(* d/dy xy^2 = x2y *)
assert (Multiplication (Variable "x", Multiplication (Value 2, Variable "y")) = begin
    deriv "y"
      (Multiplication
         (Variable "x",
          Exponentiation (Variable "y", 2))) |> simplify
  end);;
