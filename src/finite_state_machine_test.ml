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

module HAPlusRegex : Finite_state_machine.S = struct
  type state = Start | H | Success | Failure
  type symbol = char

  let transition state symbol = match (state, symbol) with
    | (Start, 'H') -> H
    | (Start, _) -> Failure

    | (H, 'A') -> Success
    | (H, _) -> Failure

    | (Success, 'H') -> H
    | (Success, _) -> Failure

    | (Failure, _) -> Failure

  let () =
    assert (Success = List.fold_left transition Start ['H'; 'A']);
    assert (Success = List.fold_left transition Start ['H'; 'A'; 'H'; 'A']);
    assert (Success = List.fold_left transition Start ['H'; 'A'; 'H'; 'A'; 'H'; 'A']);

    assert (Success != List.fold_left transition Start ['K']);
    assert (Success != List.fold_left transition Start ['H'; 'A'; 'Z']);
    assert (Success != List.fold_left transition Start ['H'; 'A'; 'H']);
    assert (Success != List.fold_left transition Start ['T'; 'H'; 'A']);
    assert (Success != List.fold_left transition Start ['H'; 'A'; 'H'; 'A'; 'T']);
end
