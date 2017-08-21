(* Structures for representing Algebraic expressions
   and methods on them *)
type value = int
type variable = string

type expression =
  | Value of value
  | Variable of variable
  | Addition of expression * expression
  | Multiplication of expression * expression
  | Exponentiation of expression * value

let rec string_of_expression = function
  | Value v -> string_of_int v
  | Variable var -> var
  | Addition (x, y) ->
    "(" ^ (string_of_expression x) ^ " + " ^ (string_of_expression y) ^ ")"
  | Multiplication (x, y) ->
    "(" ^ (string_of_expression x) ^ " * " ^ (string_of_expression y) ^ ")"
  | Exponentiation (ex, a) ->
    "(" ^ (string_of_expression ex) ^ " ^ " ^ (string_of_int a) ^ ")"

let rec simplify ex =
  let rec simplify_once = function
    | Addition (Value a, Value b) -> Value (a + b)
    | Addition (Value 0, ex)
    | Addition (ex, Value(0)) -> simplify_once ex
    | Addition (a, b) -> Addition (simplify_once a, simplify_once b)

    | Multiplication (Value a, Value b) -> Value (a * b)
    | Multiplication (Value 0, _)
    | Multiplication (_, Value 0) -> Value 0
    | Multiplication (Value 1, ex)
    | Multiplication (ex, Value 1) -> simplify_once ex
    | Multiplication (Variable a, Variable b) when a = b -> Exponentiation (Variable a, 2)
    | Multiplication (a, b) -> Multiplication (simplify_once a, simplify_once b)

    | Exponentiation (_, 0) -> Value 1
    | Exponentiation (ex, 1) -> simplify_once ex
    | Exponentiation (ex, a) -> Exponentiation (simplify_once ex, a)

    | ex -> ex in
  let first_pass = simplify_once ex in

  (* Continue simplifying until we hit a fixpoint *)
  if ex = first_pass then ex else simplify first_pass

let rec deriv term = function
  | Value _ -> Value 0
  | Variable var -> if var = term then Value 1 else Value 0
  | Addition (a, b) -> Addition (deriv term a, deriv term b)
  | Multiplication (a, b) -> Addition (
      Multiplication (a, deriv term b),
      Multiplication (deriv term a, b)
    )
  | Exponentiation (ex, a) -> Multiplication (
      Value a,
      Multiplication (
        Exponentiation (ex, a - 1),
        deriv term ex
      )
    )
