## ocaml-data-structures

I'm learning OCaml and re-learning some data structures.

### Index

* **[Stack](https://github.com/jdan/ocaml-data-structures/blob/master/src/mystack.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/mystack_test.ml)) - A simple stack implementation that pops, pushes, and peeks.
* **[Trie](https://github.com/jdan/ocaml-data-structures/blob/master/src/trie.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/trie_test.ml)) - An implementation of a [trie](https://en.wikipedia.org/wiki/Trie) which can map strings to values in a performant way.
* **[Setoid](https://github.com/jdan/ocaml-data-structures/blob/master/src/setoid.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/setoid_test.ml)) - Contract for [setoids](http://www.tomharding.me/2017/03/09/fantas-eel-and-specification-3/), which have an equivalence operation that allows these items to be placed into _sets_. The unit tests include an implementation to "uniquify" a list of "Person" setoids.
* **[Functor](https://github.com/jdan/ocaml-data-structures/blob/master/src/functor.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/functor_test.ml)) - Haskell-type functors which implement an `fmap` function. Unit tests include implementations for `Maybe` and `Either`.
* **[Algebra](https://github.com/jdan/ocaml-data-structures/blob/master/src/algebra.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/algebra_test.ml)) - Mechanism for representing algebraic instructions, with the ability to compute their derivatives.
* **[Queue](https://github.com/jdan/ocaml-data-structures/blob/master/src/myqueue.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/myqueue_test.ml)) - An immutable queue, built from two `Stack`s.
* **[Finite State Machine](https://github.com/jdan/ocaml-data-structures/blob/master/src/finite_state_machine.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/finite_state_machine_test.ml)) - Contracts for implementing a [finite state machine](https://en.wikipedia.org/wiki/Finite-state_machine) (see unit tests for `TrafficLight` and `Turnstile` implementations). Built during [@DavidKPiano's excellent ReactRally talk](https://twitter.com/DavidKPiano/status/901211642897113088)!
* **[Wire](https://github.com/jdan/ocaml-data-structures/blob/master/src/wire.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/wire_test.ml)) - A mechanism which allows you to write procedures and populate their arguments later (, based on a really interesting [Advent of Code problem](http://adventofcode.com/2015/day/7).
* **[Binary Search Tree](https://github.com/jdan/ocaml-data-structures/blob/master/src/binary_search_tree.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/binary_search_tree_test.ml)) - A [binary search tree](https://en.wikipedia.org/wiki/Binary_search_tree) parameterized over a general `Ord` type (any struct with a `compare: t -> t -> int` operation).
* **[Red-Black Tree](https://github.com/jdan/ocaml-data-structures/blob/master/src/red_black_tree.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/red_black_tree_test.ml)) - A [Red-black tree](https://en.wikipedia.org/wiki/Red%E2%80%93black_tree) implementation, which is a more specific type of binary search tree that self-balances on insert.
* **[Levenshtein Distance](https://github.com/jdan/ocaml-data-structures/blob/master/src/levenshtein.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/levenshtein_test.ml)) - A [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance) calculator, or the number of insertions, deletions, modification required to get from one sequence to another.
* **[Binary Heap](https://github.com/jdan/ocaml-data-structures/blob/master/src/binary_heap.ml)** ([tests](https://github.com/jdan/ocaml-data-structures/blob/master/src/binary_heap_test.ml)) - A [binary heap](https://en.wikipedia.org/wiki/Binary_heap) data structure parameterized with a comparable type to define min- and max- heaps.

### Development

```
# Build all the things
make

# Run the tests
make test
```
