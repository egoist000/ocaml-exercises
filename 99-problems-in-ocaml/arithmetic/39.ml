(* A list of prime numbers. (easy) *)

let is_prime n =
  if n <= 3 then n > 1
  else if n mod 2 = 0 || n mod 3 = 0 then false
  else
    let rec aux i =
      if i * i >= n then true
      else if n mod i = 0 || n mod (i + 2) = 0 then false
      else aux (i + 6)
    in
    aux 5

let rec all_primes l u =
  if l > u then []
  else if is_prime l then l :: all_primes (l + 1) u
  else all_primes (l + 1) u
