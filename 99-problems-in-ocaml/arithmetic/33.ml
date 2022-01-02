(* Determine whether two positive integer numbers are coprime. (easy) *)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let coprime n1 n2 = gcd n1 n2 = 1
