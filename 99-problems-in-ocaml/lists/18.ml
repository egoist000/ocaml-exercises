(* Extract a slice from a list. (medium) *)

(*
 * Given two indices, i and k, the slice is the list containing the elements 
 * between the i'th and k'th element of the original list (both limits included). 
 * Start counting the elements with 0 
 * (this is the way the List module numbers elements).
 *)

let rec take n = function
    | [] -> []
    | x::rest ->
            if n < 0 then []
            else x::take (n - 1) rest

let slice lst i k =
    let rec loop index = function
        | [] -> []
        | x::rest ->
                if index = i then take (k - i) (x::rest)
                else loop (index + 1) rest
    in loop 0 lst
