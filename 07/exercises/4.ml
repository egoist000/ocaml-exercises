
type obj = Miss | Cann | Barca
type situazione = obj list * obj list

let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], [])

type azione =
    | From_left of obj list
    | From_right of obj list

    (* safe: situazione -> bool *)
    (* safe s = true se la situazione è sicura, false altrimenti *)

let safe (l, r) =
    let rec aux mc cc = function
        | [] -> mc >= cc || mc = 0
        | x::rest ->
                match x with
                | Miss -> aux (mc + 1) cc rest
                | Cann -> aux mc (cc + 1) rest
                | _ -> aux mc cc rest
    in (aux 0 0 l) && (aux 0 0 r)

let rec sposta_uno e = function
    | [] -> failwith "Impossibile"
    | x::rest ->
            if x = e then rest
            else x::sposta_uno e rest

let rec sposta da = function
    | [] -> da
    | x::rest ->
            sposta (sposta_uno x da) rest

let applica act (l, r) = 
    let res = 
        match act with
        | From_left lst -> 
                if List.length lst > 2 || lst = []
                then failwith "Impossibile"
                else (sposta_uno Barca (sposta l lst), Barca::lst @ r)
        | From_right lst ->
                if List.length lst > 2 || lst = []
                then failwith "Impossibile"
                else (sposta_uno Barca (sposta r lst), Barca::lst @ l)
    in if safe res then res
                else failwith "Impossibile"

let actions =
    let elems =
        [[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
    in (List.map (function x -> From_left x) elems)
        @ (List.map (function x -> From_right x) elems)

let from_sit situa =
    let rec aux = function
        | [] -> []
        | a::rest ->
                try applica a situa::aux rest
                with _ -> aux rest
    in aux actions

