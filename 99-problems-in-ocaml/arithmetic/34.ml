let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let coprime n1 n2 = gcd n1 n2 = 1

(* Calculate Euler's totient function φ(m). (medium) *)

let phi m =
  let rec aux c =
    if c > m then 0 else if coprime c m then 1 + aux (c + 1) else aux (c + 1)
  in
    aux 1
