(* Determine whether a given integer number is prime. (medium) *)

let is_prime n =
    if n <= 3 then n > 1
    else if n mod 2 = 0 || n mod 3 = 0 then false
    else let rec aux i =
        if (i * i) >= n then true
        else if n mod i = 0 || n mod (i + 2) = 0 then false
        else aux (i + 6)
    in aux 5

