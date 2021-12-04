(* bello: int -> int *)
(* bello n = riporta true se un numero è bello false altrimenti 
 * una cifra è bella se è 0, 3, 7 un numero è bello se la sua ultima cifra è
 * bella e la penultima non lo è. *)

let rec bello n =
    match n with
    | 0 | 3 | 7 -> true
    | 1 | 2 | 4 | 5 | 6 | 8 | 9 -> false
    | _ -> let y = abs n
    in bello (y mod 10) && not (bello((y / 10) mod 10))
