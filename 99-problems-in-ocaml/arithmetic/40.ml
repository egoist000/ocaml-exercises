(* Goldbach's conjecture. (medium) *)

let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
    n <> 1 && is_not_divisor 2;;

let rec all_primes l u =
  if l > u then []
  else if is_prime l then l :: all_primes (l + 1) u
  else all_primes (l + 1) u

let goldbach n =
  let rec aux = function
    | [] ->
        raise Not_found
    | x :: rest ->
        let res = List.filter (function e -> e + x = n) rest in
        if res = [] then aux rest else (x, List.nth res 0)
  in
  aux (all_primes 0 n)
