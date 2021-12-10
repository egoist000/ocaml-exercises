(* dizionario: ('a * 'b) list *)

let rec assoc key = function
    | [] -> raise Not_found
    | (k, v)::rest ->
            if k = key then v
            else assoc k rest

(* l'inserimento è un' operazione costosa, conviene mettere il nuovo valore
 * in testa alla lista associativa, poichè la ricerca riporta il primo valore
 * della chiave *)

let inserisci k v assoc_list = (k, v)::assoc_list

let rec cancella key = function
    | [] -> []
    | (k, v)::rest ->
            if k = key then cancella k rest
            else (k, v)::cancella k rest


