(* albero binario di ricerca -> ogni nodo è etichettato da una coppia 
 * (Chiave, Valore), per ogni nodo N le chiavi del sottoalbero sinistro di N
 * sono tutte minori della chiave associata alla radice del sottoalbero,
 * le chiavi del sottoalbero destro invece sono tutte maggiori della radice del
 * sottoalbero *)

type ('a, 'b) tree = Empty | Tr of ('a * 'b) * ('a, 'b) tree * ('a, 'b) tree

(* abr_check: ('a * 'b) tree -> bool *)
(* abr_check t = true se t è un albero binario di ricerca, false altrimenti *)

let rec for_all p = function
  | Empty ->
      true
  | Tr (k, l, r) ->
      p k && for_all p l && for_all p r

let rec abr_check = function
  | Empty ->
      true
  | Tr ((k, _), l, r) ->
      for_all (function k1, _ -> k1 < k) l
      && for_all (function k1, _ -> k1 > k) r
      && abr_check l && abr_check r

(* abr_search: ('a * 'b) tree -> 'a -> 'b *)
(* abr_search t k = riporta il valore associato alla chiave in un abr, un errore
 * se non esiste *)

let rec abr_search t key =
  match t with
  | Empty ->
      failwith "Errore"
  | Tr ((k, v), l, r) ->
      if k = key then v
      else if k < key then abr_search l key
      else abr_search r key

(* abr_update: ('a * 'b) tree -> ('a * 'b) -> ('a * 'b) tree *)
(* abr_update t (k, v) = un albero binario di ricerca che si ottiene da t
 * aggiungendo la coppia (k, v) oppure, se gia presente sostituire il valore
 * associato a k con v *)

let rec abr_update t (key, value) =
  match t with
  | Empty ->
      Tr ((key, value), Empty, Empty)
  | Tr ((k, v), l, r) ->
      if k = v then Tr ((k, value), l, r)
      else if k < key then Tr ((k, v), abr_update l (key, value), r)
      else Tr ((k, v), l, abr_update r (key, value))

(* abr_delim: 'a tree -> 'a * 'a tree *)
(* abr_delim t = una coppia (label, tree) dove label è una coppia chiave, valore
 * con la chiave minima di t e il suo valore associato, mentre tree è l'albero
 * che si ottiene da t eliminando il nodo che contiene la chiave minima *)

let rec abr_delmin = function
  | Empty ->
      failwith "Error"
  | Tr (x, Empty, r) ->
      (x, r)
  | Tr (x, l, r) ->
      let y, newl = abr_delmin l in
      (y, Tr (x, newl, r))

(* abr_delete: ('a * 'b) tree -> 'a -> ('a * 'b) tree *)

let rec abr_delete t key =
  match t with
  | Empty ->
      Empty
  | Tr ((k, _), Empty, r) ->
      if k = key then r else abr_delete r key
  | Tr ((k, _), l, Empty) ->
      if k = key then l else abr_delete l key
  | Tr ((k, _), l, r) ->
      if key < k then abr_delete l key
      else if key > k then abr_delete r key
      else
        let lbl, tr = abr_delmin r in
        Tr (lbl, l, tr)
