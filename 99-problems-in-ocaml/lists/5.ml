(* Reverse a list. (easy) *)

let rev lst = 
    let rec loop tmp = function
        | [] -> tmp
        | x::rest ->
                loop (x::tmp) rest
    in loop [] lst

(* rev ["a"; "b"; "c"];;
 * - : string list = ["c"; "b"; "a"] *)
