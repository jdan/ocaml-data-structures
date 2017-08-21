(* An immutable queue! *)

(* We'll build our queue out of two Mystacks *)
type 'a queue = 'a Mystack.t * 'a Mystack.t

let empty = (Mystack.Empty, Mystack.Empty)

(* Queueing is easy: we just push an item to the front stack *)
let enqueue item ((front, back) :'a queue) : 'a queue =
  (Mystack.push front item, back)

(* Dequeueing is where our "back" stack somes in hand. We can pop
   from the "back" to get our items in reverse, but if "back" is
   empty then we need to "spill" the front stack into the back. *)
let rec dequeue (front, back) =
  (* Spill the front stack into the back - it'll be reversed *)
  let rec spillover (front, back) = match Mystack.pop front with
    | None -> (front, back)
    | Some (item, tail) -> spillover (tail, Mystack.push back item) in

  if empty = (front, back) then None
  else match Mystack.pop back with
    | None -> dequeue @@ spillover (front, back)
    | Some (item, tail) -> Some (item, (front, tail))
