(* Determine the prime factors of a given positive integer (2). (medium) *)

(* Construct a list containing the prime factors and their multiplicity *)
let factors n =
  let rec aux c n =
    if n = 1 then []
    else if n mod c = 0 then
      match aux c (n / c) with
      | (h, m) :: rest when h = c ->
          (h, m + 1) :: rest
      | l ->
          (c, 1) :: l
    else aux (c + 1) n
  in
  aux 2 n
