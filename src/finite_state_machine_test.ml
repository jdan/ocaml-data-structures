module TrafficLight : Finite_state_machine.S = struct
  type state = Green | Yellow | Red
  type symbol = Wait

  let transition state symbol = match (state, symbol) with
    | (Green, Wait) -> Yellow
    | (Yellow, Wait) -> Red
    | (Red, Wait) -> Green

  let () =
    assert (Green = List.fold_left transition Green [Wait; Wait; Wait]);
    assert (Red = List.fold_left transition Green [Wait; Wait])
end

module Turnstile : Finite_state_machine.S = struct
  type state = Locked | Unlocked
  type symbol = InsertCoin | Push

  let transition state symbol = match (state, symbol) with
    | (Locked, InsertCoin) -> Unlocked
    | (Locked, Push) -> Locked
    | (Unlocked, Push) -> Locked
    | (Unlocked, InsertCoin) -> Unlocked

  let () =
    assert (Unlocked = List.fold_left transition Locked [InsertCoin]);
    assert (Locked = List.fold_left transition Locked [InsertCoin; Push; Push]);
end
