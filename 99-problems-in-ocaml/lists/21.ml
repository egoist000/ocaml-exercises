(* Insert an element at a given position into a list. (easy) *)

(*
 * Start counting list elements with 0. If the position is larger or equal to the 
 * length of the list, insert the element at the end. 
 * (The behavior is unspecified if the position is negative.)
 *)

let rec insert_at e i = function
    | [] -> [e]
    | x::rest ->
            if i = 0 then e::x::rest
            else x::insert_at e (i - 1) rest
