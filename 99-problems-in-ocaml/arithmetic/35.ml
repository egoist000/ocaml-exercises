(* Determine the prime factors of a given positive integer. (medium) *)

let factors n =
  let rec aux c n =
    if n = 1 then []
    else if n mod c = 0 then c :: aux c (n / c)
    else aux (c + 1) n
  in
  aux 2 n
