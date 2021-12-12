let in_labirinto dim (r, c) =
    r >= 0 && c >= 0 && r < dim && c < dim

(* filter_vicini: int -> (int * int ) list -> (int * int) list *)
(* filter_vicini dim clist = la lista di caselle nel labirinto *)
let filter_vicini dim clist =
    (* loop: (int * int) list -> (int * int) list *)
    (* loop tmp = lista di caselle raggiungibili::tmp *)
    let rec loop tmp = function
        | [] -> tmp
        | casella::rest ->
                if in_labirinto dim casella
                then loop (casella::tmp) rest
                else loop tmp rest
    in loop [] clist

(* combine: 'a list -> 'b list -> ('a * 'b) list *)
(* combine xlist ylist = una lista di coppie (x1, y1)..(xn, yn).
 * Solleva un eccezione se le liste hanno lunghezze diverse *)

let combine xlist ylist =
    if List.length xlist != List.length ylist
    then failwith "Error"
    else let rec aux xlst ylst =
        match xlst, ylst with
        | x::[], y::[] -> [(x, y)]
        | x::xrest, y::yrest ->
                (x, y)::aux xrest yrest
        | _, _ -> failwith "Error"
    in aux xlist ylist

let rec combine' xlist ylist =
    match xlist, ylist with
    | x::[], y::[] -> [(x, y)]
    | x::xrest, y::yrest ->
            (x, y)::combine xrest yrest
    | _, _ -> failwith "Error"

(* split: ('a * 'b) list -> 'a list * 'b list *)
(* split lst = una coppia di liste contenente come primo elemento tutti i primi
 * elementi di lst e come secondo tutti i secondi elementi di lst *)

let rec split = function
    | [] -> ([], [])
    | (x, y)::rest ->
            (x::fst(split rest), y::snd(split rest))

(* cancella: 'a -> ('a * 'b) list -> ('a * 'b) list *)
(* cancella key lst = rimuove tutte le coppie nella lista associativa che 
 * hanno come chiave key *)

let rec cancella key = function
    | [] -> []
    | (k, v)::rest ->
            if key = k then cancella key rest
            else (k, v)::cancella key rest


