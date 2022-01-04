(*
 * Un albero di minimax è un albero n-ario le cui foglie sono etichettate da interi,
 * e i nodi intermedi sono etichettati da una coppia di elementi, costituita da uno
 * dei due valori Min o Max e da un intero.
 * L’intero è detto valore numerico del nodo (intermedio o foglia).
 * Rappresentiamo gli alberi di minimax mediante le seguenti dichiarazioni di
 * tipo:
 *)

type player = Min | Max

type minmaxtree = Leaf of int | Node of (player * int) * minmaxtree list

(* propagate: minmaxtree -> minmaxtree *)

(*
 * applicata a un qualsiasi albero T di minimax, restituisca l’albero che si
 * ottiene da T sostituendo (eventualmente) i valori dei nodi intermedi in modo
 * che siano ben assegnati.
 *)

(* valore minimo di una lista *)
(* minlst: 'a list -> 'a *)
(* minlst lst = il minimo valore in lst *)

let rec minlst = function
  | [] ->
      failwith "minlst"
  | [x] ->
      x
  | x :: y :: rest ->
      minlst (min x y :: rest)

(* massimo valore in una lista *)
(* maxlst: 'a list -> 'a *)
(* maxlst lst = il massimo valore in lst *)

let rec maxlst = function
  | [] ->
      failwith "maxlst"
  | [x] ->
      x
  | x :: y :: rest ->
      maxlst (max x y :: rest)

(* value: minmaxtree -> int *)
(* value t = valore numerico della radice t *)

let rec value = function Leaf x -> x | Node ((_, x), _) -> x

let rec propagate = function
  | Leaf x ->
      Leaf x
  | Node ((p, _), tlst) ->
      let subtrees = List.map propagate tlst in
      let v = (if p = Min then minlst else maxlst) (List.map value subtrees) in
      Node ((p, v), subtrees)

(* minmaxtree di esempio *)
let tree =
  Node
    ( (Max, 0)
    , [ Node ((Min, 100), [Leaf 3; Leaf 12; Leaf 8])
      ; Node ((Min, 0), [Leaf 2; Leaf 4; Leaf 6])
      ; Node ((Min, 30), [Leaf 14; Leaf 15; Leaf 10]) ] )
