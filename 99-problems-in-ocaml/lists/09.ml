(* Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack lst = 
    let rec aux key tmp = function
        | [] -> [tmp]
        | x::rest ->
                if x <> key then tmp::aux x [x] rest
                else aux key (x::tmp) rest
    in try aux (List.hd lst) [] lst
    with _ -> failwith "Empty_List"

(* # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; 
 *         "d"; "d"; "e"; "e"; "e"; "e"];;
 * - : string list list =
 * [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 * ["e"; "e"; "e"; "e"]] *)
