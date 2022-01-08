(* rappresentazione di un grafo mediante liste di successori *)
type 'a graph1 = ('a * 'a list) list

let grafo =
  [(1, [2; 3; 4]); (2, [6]); (3, [5]); (4, [6]); (5, [4]); (6, [5; 7]); (7, [])]

(* successori: 'a -> 'a graph1 -> 'a list *)
(* successori x grafo = la lista di successori di x nel grafo, solleva un 
 * eccezione se x non è un grafo *)

let rec successori x grafo = try List.assoc x grafo with _ -> raise Not_found

(* rappresentazione di un grafo mediante lista di archi *)
(* simile alla rappresentazione precedente, cambia l'algoritmo per trovare i
 * succcessori di un nodo *)

type 'a graph2 = ('a * 'a) list

let grafo2 =
  [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (5, 4); (6, 5); (6, 7)]

(* successori2: 'a -> 'a graph2 -> 'a list *)
(* successori2 = lo stesso di successori *)
let rec successori2 x = function
  | [] ->
      []
  | (n, y) :: rest ->
      if x = n then y :: successori2 x rest else successori2 x rest

(* versione che utilizza le funzioni di ordine superiore *)
let successori3 x grafo =
  List.map snd (List.filter (function (n, _) -> x = n) grafo)

(* Nel caso di un grafo non orientato rappresentato come lista di archi *)

(* vicini: 'a -> 'a graph2 -> 'a list *)
(* vicini x grafo = i vicini di un nodo in un grafo non orientato *)

let rec vicini x = function
  | [] ->
      []
  | (k, y) :: rest ->
      if x = k then y :: vicini x rest
      else if y = x then k :: vicini x rest
      else vicini x rest
