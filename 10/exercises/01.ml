(* definizione di un grafo mediante lista di archi *)

type 'a graph = ('a * 'a) list

let grafo =
  [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (5, 4); (6, 5); (6, 7)]

let successori x g = List.map snd (List.filter (function e, _ -> e = x) g)

(* test_connessi: 'a graph -> 'a -> 'a -> bool *)
(* test_connessi g n m = true se esiste un cammino da N a M nel grafo g,
 * false altrimenti *)

let test_connessi g n m =
  let rec search visited = function
    | [] ->
        false
    | x :: rest ->
        if List.mem x visited then search visited rest
        else if x = m then true
        else search (x :: visited) (successori x g @ rest)
  in
  search [] [n]
