(* prendi ('a -> bool) -> 'a list -> 'a * 'a list *)
(* prendi p lst = applicata a un predicato p ritorna la coppia (x, l')
 * dove x è il primo elemento di lst che soddisfa p e l' sono tutti 
 * gli altri elementi di lst *)

let rec prendi p = function
    | [] -> failwith "Error"
    | k::rest ->
            if p k then (k, rest)
            else let (x, l) = prendi p rest
            in (x, k::l)

