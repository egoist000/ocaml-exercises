(* quicksort implementation *)

let rec partition pivot = function
    | [] -> ([], [])
    | x::rest ->
            let (left, right) = partition pivot rest
            in if pivot <= x then (left, x::right)
            else (x::left, right)

let rec quicksort = function
    | [] -> []
    | x::rest ->
            let (left, right) = partition x rest
            in (quicksort left) @ (x::quicksort right)
