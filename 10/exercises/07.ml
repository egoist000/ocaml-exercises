type col = Rosso | Giallo | Verde | Blu

type 'a col_assoc = (col * 'a list) list

type 'a graph = ('a * 'a) list

let successori x g = List.map snd (List.filter (function c, _ -> c = x) g)

let rec get_color n = function
  | [] ->
      raise Not_found
  | (col, lst) :: rest ->
      if List.mem n lst then col else get_color n rest

(* colori_alterni: 'a graph -> 'a col_assoc -> 'a -> 'a -> 'a list *)
(* colori_alterni g col_lst start goal = una lista che rappresenta un cammino 
 * da start a goal nel grafo g con colori alterni, solleva un eccezione se tale
 * cammino non esiste *)

let colori_alterni (nodes, arcs) col_assoc start goal =
  let rec from_node a prevcol visited =
    let curr_color = get_color a col_assoc in
    if List.mem a visited || prevcol = curr_color then raise Not_found
    else if a = goal then [goal]
    else a :: from_list (a :: visited) (successori a arcs)
  and from_list visited = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try from_node x (get_color x col_assoc) visited
      with _ -> from_list visited rest )
  in
  start :: from_node (List.nth (successori start arcs) 0) (get_color start) []
