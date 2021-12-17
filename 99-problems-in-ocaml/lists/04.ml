(* Find the number of elements of a list. (easy) *)

let rec length = function
    | [] -> 0
    | x::rest ->
            1 + length rest

(* tail recursive *)

let length' lst =
    let rec loop len = function
        | [] -> len 
        | x::rest ->
                loop (len + 1) rest
    in loop 0 lst

(* # length ["a"; "b"; "c"];;
 * - : int = 3
 * # length [];;
 * - : int = 0 *)
