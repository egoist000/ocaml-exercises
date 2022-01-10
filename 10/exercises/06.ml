type 'a graph = 'a list * ('a * 'a) list

let successori x g = List.map snd (List.filter (function c, _ -> c = x) g)

let grafo =
  ( [1; 2; 3; 4; 5; 6; 7]
  , [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (5, 4); (6, 5); (6, 7)] )

(* remove: 'a -> 'a list *)
(* remove e = una lista contenente tutti gli elementi originali tranne E se 
 * presente *)

let rec remove e = function
  | [] ->
      []
  | x :: rest ->
      if e = x then rest else x :: remove e rest

(* cammino: 'a graph -> 'a list -> 'a -> 'a -> 'a list *)
(* cammino g lst n m = riporta un cammino da n a m che passi per i nodi di 
 * lst esattamente una volta, un eccezione se tale cammino non esiste *)

let cammino (_, arcs) lst n m =
  let rec from_node a lst =
    if not (List.mem a lst) then raise Not_found
    else if a = m && lst = [m] then [m]
    else
      let newlst = remove a lst in
      a :: from_list newlst (successori a arcs)
  and from_list nodelst = function
    | [] ->
        raise Not_found
    | n :: nrest -> (
      try from_node n nodelst with Not_found -> from_list nodelst nrest )
  in
  from_node n lst

let hamiltoniano ((nodes, arcs) as g) =
  let node = List.nth nodes (Random.int (List.length nodes)) in
  let rec from_list lst = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try cammino g lst x node with Not_found -> from_list lst rest )
  in
  node :: from_list nodes (successori node arcs)
