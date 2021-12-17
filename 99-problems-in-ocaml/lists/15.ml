(* Replicate the elements of a list a given number of times. (medium) *)

let rec upto e = function
    | 0 -> []
    | n -> e::upto e (n - 1)

let rec replicate lst n =
    match lst with
    | [] -> []
    | x::rest -> upto x n @ replicate rest n
