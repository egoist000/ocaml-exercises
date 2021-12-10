(** Somma sottoinsiemi utilizzando il backtracking **)

(** utility **)

exception NotFound

(* sumof: int list -> int *)
(* sumof lst = somma degli elementi della lista *)

let rec sumof = function
    | [] -> 0
    | x::rest -> x + sumof rest

(* subset_search: int list -> int -> int list *)
(* aux: int list -> int list -> int list *)
(* aux solution altri = lista @ solution, dove lista contiene numeri 
 * scelti da altri, tali che sumof (lista @ solution) = n *)

let subset_search set n =
    let rec aux solution altri =
        let somma = sumof solution in
        if somma > n then raise NotFound
        else if somma = n then solution
        else
            match altri with
            | [] -> raise NotFound
            | x::rest ->
                    try aux (x::solution) rest with
                    | NotFound -> aux solution rest
    in aux [] set

let rec stampa_soluzione = function
    | [] -> print_string "\n"
    | [x] -> print_int x; print_string "\n"
    | x::y::rest -> 
            print_int x; print_string ", ";
            stampa_soluzione (y::rest)

let rec subset_search_printall set n =
    let rec aux solution altri =
        let somma = sumof solution
        in if somma > n then raise NotFound
        else if somma = n 
        then begin
            stampa_soluzione solution;
            raise NotFound
        end
        else match altri with
        | [] -> raise NotFound
        | x::rest ->
                try aux (x::solution) rest with
                 | NotFound -> aux solution rest
    in try aux [] set with
    | NotFound -> ()
