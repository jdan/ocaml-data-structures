open Myqueue;;

let q = empty |> enqueue 1 |> enqueue 5 |> enqueue 17;;

match dequeue q with
| None -> assert false
| Some (v1, q1) -> (
    (* Make sure our first value is a 1 *)
    assert (v1 = 1);
    match dequeue q1 with
    | None -> assert false
    | Some (v2, q2) -> (
        assert (v2 = 5);
        let q3 = enqueue 20 q2 in
        match dequeue q3 with
        | None -> assert false
        | Some (v4, q4) -> (
            assert (v4 = 17);
            match dequeue q4 with
            | None -> assert false
            | Some (v5, q5) -> (
                (* Make sure queueing that 20 in the middle worked, and
                   that we can no longer dequeue *)
                assert (v5 = 20);
                assert (None = dequeue q5);
              )
          )
      )
  )
