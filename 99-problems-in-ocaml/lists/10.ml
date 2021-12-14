(* Run-length encoding of a list. (easy) *)

let encode lst =
    let rec aux c k = function
        | [] -> [(c, k)]
        | x::rest ->
                if x = k then aux (c + 1) k rest
                else (c, k)::aux 1 x rest
    in try aux 0 (List.hd lst) lst
    with _ -> failwith "Empty_List"
