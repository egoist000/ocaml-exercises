(* Duplicate the elements of a list. (easy) *)

let rec duplicate = function
    | [] -> []
    | x::rest -> x::x::duplicate rest
