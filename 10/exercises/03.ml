type 'a graph = ('a * 'a) list

let grafo =
  [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (5, 4); (6, 5); (6, 7)]

let successori x g = List.map snd (List.filter (function e, _ -> e = x) g)

(* ciclo: 'a graph -> 'a -> 'a list *)
(* ciclo g x = la lista di nodi che rappresentano un ciclo su x in g *)

let ciclo g x =
  let rec from_node visited a =
    if List.mem a visited then raise Not_found
    else if a = x then [a]
    else a :: from_list (a :: visited) (successori a g)
  and from_list visited = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try from_node visited x with _ -> from_list visited rest )
  in
  x :: from_list [] (successori x g)
