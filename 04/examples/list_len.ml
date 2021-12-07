(* list_len: a' list -> int *)
(* list_len lst = la lunghezza della lista *)

let rec list_len lst =
    if lst = [] then 0
    else 1 + list_len (List.tl lst)

let rec list_len' = function
    | [] -> 0
    | _::rest -> 1 + list_len' (rest)
