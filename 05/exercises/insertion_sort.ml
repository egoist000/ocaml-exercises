(* insertionsort implementation in ocaml *)

let rec insert k = function
    | [] -> [k]
    | y::rest ->
            if k <= y then k::y::rest
            else y::insert k rest

let rec insertionsort = function
    | [] -> []
    | x::rest ->
            insert x (insertionsort rest)
