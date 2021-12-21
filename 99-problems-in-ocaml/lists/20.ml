(* Remove the K'th element from a list. (easy) *)

let rec remove_at n = function
    | [] -> []
    | x::rest ->
            if n = 0 then rest
            else x::remove_at (n - 1) rest
