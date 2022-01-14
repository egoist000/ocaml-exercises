(* Calculate Euler's totient function φ(m) (improved). (medium) *)

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

(* e >= 0 *)

let rec pow b e = if e = 0 then 1 else b * pow b (e - 1)

let phi_improved n =
  let lst = factors n in
  let rec aux = function
    | [] ->
        1
    | (p, m) :: rest ->
        (p - 1) * pow p (m - 1) * aux rest
  in
  aux lst
