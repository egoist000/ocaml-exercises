(* Drop every N'th element from a list. (medium) *)

let drop lst n =
    let rec aux i = function
        | [] -> []
        | x::rest ->
                if i = n then aux 1 rest
                else x::aux (i + 1) rest
    in aux 1 lst
