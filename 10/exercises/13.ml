type 'a graph = ('a * 'a) list

let successori x g = List.map snd (List.filter (function c, _ -> c = x) g)

let pari n = n >= 0 && n mod 2 = 0

let grafo =
  [(1, 3); (2, 6); (3, 4); (3, 5); (3, 6); (4, 2); (4, 5); (5, 4); (6, 5)]

(* path_n_p: 'a graph -> ('a -> bool) -> int -> 'a -> 'a list *)
(* path_n_p g p n start = cammino non ciclico che parte da start e contiene 
 * esattamente n nodi che soddisfano p *)

let path_n_p g p n start =
  let rec from_node count a visited =
    if count <= 0 || List.mem a visited then raise Not_found
    else if count = 1 && p a then [a]
    else if count > 1 && p a then
      a :: from_list (count - 1) (a :: visited) (successori a g)
    else a :: from_list count (a :: visited) (successori a g)
  and from_list c visited = function
    | [] ->
        raise Not_found
    | n :: nrest -> (
      try from_node c n visited with _ -> from_list c visited nrest )
  in
  from_node n start []
