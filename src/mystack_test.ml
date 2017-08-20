open Mystack;;

assert (push Empty 5 = Item (5, Empty));;
assert (push (Item (5, Empty)) 6 = Item (6, Item (5, Empty)));;

assert (pop Empty = None);;
assert (pop (Item (10, Item (4, Empty))) = Some (10, Item (4, Empty)));;

assert (peek Empty = None);;
assert (peek (Item (10, Item (4, Empty))) = Some 10);;

assert (stack_of_list [1; 2; 3] = Item (1, Item (2, Item (3, Empty))));;
assert (stack_of_list [] = Empty);;

assert (list_of_stack (Item (5, Item (2, Item (10, Empty)))) = [5; 2; 10]);;
assert (list_of_stack Empty = []);;
