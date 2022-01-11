type 'a graph = ('a * 'a) list

let successori x g = List.map snd (List.filter (function c, _ -> c = x) g)

let grafo =
  [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (6, 5); (6, 7); (5, 4)]

let rec remove e = function
  | [] ->
      []
  | x :: rest ->
      if x = e then rest else x :: remove e rest

(* cammino_con_nodi: 'a graph -> 'a -> 'a list -> 'a list *)
(* cammino_con_nodi g start nodelst = la lista che rappresenta il cammino senza
 * cicli da start e che contenga tutti i nodi di nodelst ed 
 * eventualmente altri *)

let cammino_con_nodi g start nodelst =
  let rec from_node a lst visited =
    if List.mem a visited then raise Not_found
    else if lst = [] then [a]
    else a :: from_list (a :: visited) lst (successori a g)
  and from_list visited lst = function
    | [] ->
        raise Not_found
    | n :: nrest -> (
      try from_node n (remove n lst) visited
      with _ -> from_list visited lst nrest )
  in
  from_node start nodelst []
