(* Eliminate consecutive duplicates of list elements. (medium) *)

let compress lst =
    let rec aux key = function
        | [] -> []
        | x::rest ->
                if key = x then aux key rest
                else x::aux x rest
    in try (List.hd lst)::(aux (List.hd lst) (List.tl lst))
    with _ -> []

(* # compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
 * - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
