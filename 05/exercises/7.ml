(* strike_ball 'a list -> 'a list -> (int * int) *)

let strike_ball test guess =
    let rec aux t g =
        match t, g with
        | [], [] -> (0, 0)
        | t::trest, g::grest ->
                let sbrest = aux trest grest in 
                if t = g 
                then (fst(sbrest), 1 + snd(sbrest))
                else if List.mem t guess
                then (1 + fst(sbrest), snd(sbrest))
                else (fst(sbrest), snd(sbrest))
        | _, _ -> failwith "Error"
    in aux test guess
