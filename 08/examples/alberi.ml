(* definizione di un albero binario *)

type 'a tree =
    | Leaf of 'a
    | One of 'a * 'a tree
    | Two of 'a * 'a tree * 'a tree

(* calcolo della dimensione di un albero binario *)
(* size: 'a tree -> int *)
(* size t = numero di nodi in t *)

let rec size = function
    | Leaf _ -> 1
    | One (_, x) -> 1 + size x
    | Two (_, x, y) -> 1 + (size x + size y)

(* definizione di albero migliorata, ogni albero diverso da Empty 
 * ha esattamente due sottoalberi *)

type 'a albero =
    | Empty
    | Tr of 'a * 'a albero * 'a albero

let albero = 
    Tr(1, Tr(2, Tr(2,Empty,Empty), Empty), 
	  Tr(3, Tr(5, Tr(1,Empty,Empty), Tr(1,Empty,Empty)),
		Empty)) 

let is_empty = function
    | Empty -> true
    | _ -> false

exception EmptyTree 
let root = function
    | Empty -> raise EmptyTree
    | Tr(x, _, _) -> x

let is_leaf = function
    | Tr(x, Empty, Empty) -> true
    | _ -> false

let leaf x = Tr(x, Empty, Empty)

let left = function
    | Empty -> raise EmptyTree
    | Tr (_, l, _) -> l

let right = function
    | Empty -> raise EmptyTree
    | Tr(_, _, r) -> r

let rec size = function
    | Empty -> 0
    | Tr(_, l, r) -> 1 + size l + size r

let rec size' t = 
    if is_empty t then 0
    else 1 + size (left t) + size (right t)

(* contare le occorrenze di un etichetta in un albero binario *)

(* sottoproblema: operazione da compiere quando si visita un nodo *)
(* add : 'a -> ('a * int) list -> ('a * int) list *)
(* add y lst = lista che si ottiene da lst sostituendo la 
               coppia (y,n) con (y,n+1), se una tale coppia esiste, 
               altrimenti aggiungendo la coppia (y,1) *)

let rec add y = function
    | [] -> [(y, 1)]
    | (x, n)::rest ->
            if x = y then (y, n + 1)::rest
            else (x, n)::add y rest

(* count: 'a tree -> ('a * int) list *)
(* count t = lista di coppie contenenti per ogni etichetta nell'albero 
 * il numero di volte che occorre in quest'ultimo *)

let count t =
    let rec aux res = function
        | Empty -> res
        | Tr(a, l, r) ->
                aux (aux (add a res) l) r
    in aux [] t

