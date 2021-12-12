(* range: int -> int list *)
(* range n = una lista di interi [1..n] *)

let range n =
    let rec loop i =
        if i > n then []
        else i::loop (i + 1)
    in loop 1

(* intpairs: int -> (int * int) list *)
(* intpairs n = una lista di coppie che applicata ad un numero positivo n 
 * riporti tutte le coppie (x, y) con x e y compresi tra 1 e n *)

let intpairs n =
    (* assoclst: int -> int list -> (int * int) list *)
    (* assoclst y xlst = una lista di coppie (y, x1)..(y, xn) *)
    let rec assoclst y = function
        | [] -> []
        | x::rest ->
                (y, x)::assoclst y rest
    (* aux: int -> (int * int) list *)
    (* aux iter = una lista di coppie dove il primo elemento è uguale a iter 
     * @ alle altre liste delle iterazioni successive *)
    in let rec aux iter =
        if iter > n then []
        else (assoclst iter (range n)) @ aux (iter + 1)
    in aux 1

