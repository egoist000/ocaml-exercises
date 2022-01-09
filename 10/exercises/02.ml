type 'a graph = ('a * 'a) list

let successori x g = List.map snd (List.filter (function e, _ -> e = x) g)

let grafo =
  [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (5, 4); (6, 5); (6, 7)]

(* esiste_ciclo: 'a graph -> 'a -> bool *)
(* esiste_ciclo g x = true se a partire da x nel grafo g esiste un ciclo,
 * false altrimenti *)

let esiste_ciclo g start =
  let rec search visited = function
    | [] ->
        false
    | x :: rest ->
        if List.mem x visited then search visited rest
        else x = start || search (x :: visited) (successori x g @ rest)
  in
  search [] (successori start g)
