(* Finite state machines! *)

(* We'll provide a contract for implementing FSM's - see _test.ml *)
module type S = sig
  type state
  type symbol
  val transition : state -> symbol -> state
end
