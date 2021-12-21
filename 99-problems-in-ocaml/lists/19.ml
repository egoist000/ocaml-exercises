(* Rotate a list N places to the left. (medium) *)

let rotate lst n =
    let rec aux tmp i = function
        | [] -> tmp
        | x::rest ->
                if i <= 0 then x::(rest @ (List.rev tmp))
                else aux (x::tmp) (i - 1) rest
    in 
    if n < 0 
    then aux [] ((List.length lst) + n) lst
    else aux [] n lst
