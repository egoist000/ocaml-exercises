(* apply: ('a * 'a) list -> 'a list -> 'a list *)
(* apply asslist elle = una lista che si ottiene da elle sostituendo ogni
 * elemento che occorre come chiave in asslist il suo valore e lasciando gli 
 * altri inalterati *)

let rec apply asslist = function
  | [] ->
      []
  | x :: rest ->
      let value = try List.assoc x asslist with Not_found -> x in
      value :: apply asslist rest

let lista_associativa = [(2, 4); (3, 6); (5, 10); (8, 16); (11, 22)]

let lista_prova = [4; 5; 7; 11]

(*
 * # apply lista_associativa lista_prova;;
 * - : int list = [4; 10; 7; 22]
 *)
