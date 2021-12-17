 (* Split a list into two parts; the length of the first part is given. (easy) *)

(*
 * If the length of the first part is longer than the entire list, 
 * then the first part is the list and the second part is empty.
 *)

let split lst n =
    let rec aux c = function
        | [] -> ([], [])
        | x::rest ->
                if c = 0 then ([], rest)
                else let res = aux (c - 1) rest
                in (x::fst res, snd res)
    in aux n lst

