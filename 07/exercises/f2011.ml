type 'a pattern = Jolly | Val of 'a

(* most_general_match: ’a list -> ’a list -> ’a pattern list *)

let rec most_general_match l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | x::xrest, y::yrest ->
            if x = y then Val x::most_general_match xrest yrest
            else Jolly::most_general_match xrest yrest
    | _ -> failwith "Error"
