type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

type 'a sostituzione = ('a * 'a tree) list

(* applica: 'a sostituzione -> 'a tree -> 'a tree *)
(* applica sostituzione tree = un albero a cui ogni foglia targata con x è stato 
 * sostituito l'albero associato a quel valore nella lista di coppie *)

let rec applica sost = function
  | Empty ->
      Empty
  | Tr (x, Empty, Empty) as foglia -> (
    try List.assoc x sost with _ -> foglia )
  | Tr (x, l, r) ->
      Tr (x, applica sost l, applica sost r)
