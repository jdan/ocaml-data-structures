let min3 x y z = min (min x y) z

let lev a b =
  let rec inner i j =
    (* Either string empty -> `max i j` insertions *)
    if min i j = 0 then max i j
    else
      min3
        (* I: delete char from a *)
        (1 + inner (i - 1) j)
        (* II: delete char from b *)
        (1 + inner i (j - 1))
        (* III: delete char from both *)
        (inner (i - 1) (j - 1) +
         (* What is the cost of changing i'th of a to j'th of b? *)
         (if a.(i - 1) = b.(j - 1) then 0 else 1))

  in inner (Array.length a) (Array.length b)
