(* Find the K'th element of a list. (easy) *)

let rec at n = function
    | [] -> None
    | x::rest ->
            if n < 1 then None
            else if n = 1 then Some x
            else at (n - 1) rest

let at' n lst = 
    try Some (List.nth lst (n - 1)) with
    | _ -> None

(* # at 3 ["a"; "b"; "c"; "d"; "e"];;
 * - : string option = Some "c"
 * # at 3 ["a"];;
 * - : string option = None *)
