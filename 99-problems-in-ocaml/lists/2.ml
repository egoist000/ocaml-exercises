(* Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two = function
    | [] | [_] -> None
    | x::y::[] -> Some (x, y)
    | _::rest -> last_two rest

(* # last_two ["a"; "b"; "c"; "d"];;
 * - : (string * string) option = Some ("c", "d")
 * # last_two ["a"];;
 * - : (string * string) option = None *)
