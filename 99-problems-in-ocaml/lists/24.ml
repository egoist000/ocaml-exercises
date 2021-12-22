(* Lotto: Draw N different random numbers from the set 1..M. (easy) *)

let rec lotto_select n m =
  if n <= 0 then []
  else
    let rnd =(Random.int m) + 1 in (* random number between 1 and m*)
    rnd :: lotto_select (n - 1) m
