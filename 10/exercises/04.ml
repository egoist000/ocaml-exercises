(* rappresentazione di un grafo non orientato, lista di nodi, lista di archi *)
type 'a graph = 'a list * ('a * 'a) list

let grafo =
  ( [1; 2; 3; 4; 5; 6; 7]
  , [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (5, 4); (6, 5); (6, 7)] )

(* funzione che ritorna la lista dei vicini di un nodo in un
 * grafo non orientato *)

let rec vicini x = function
  | [] ->
      []
  | (y, z) :: rest ->
      if x = y then z :: vicini x rest
      else if x = z then y :: vicini x rest
      else vicini x rest

(* exist_path ('a * 'a) list -> 'a -> 'a -> bool *)
(* exist_path a n m = true se esiste un cammino da n a m nel grafo non orientato
 * , false altrimenti *)

let exist_path g n m =
  let rec search visited = function
    | [] ->
        false
    | x :: rest ->
        if List.mem x visited then search visited rest
        else x = m || search (x :: visited) (vicini x g @ rest)
  in
  search [] (vicini n g)

(* grafo_connesso: 'a graph -> bool *)
(* grafo_connesso g = true se il primo nodo del grafo (se esiste) è connesso 
 * con tutti gli altri nodi del grafo, false altrimenti *)
(** se vale questa proprietà allora il grafo è connesso **)

let grafo_connesso (nodes, arcs) =
  match nodes with
  | [] ->
      false
  | n :: nrest ->
      List.for_all (exist_path arcs n) nrest
