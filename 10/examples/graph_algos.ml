(* rappresentazione di un grafo mediante lista di archi *)
type 'a graph = ('a * 'a) list

let grafo =
  [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (5, 4); (6, 5); (6, 7)]

let rec successori x = function
  | [] ->
      []
  | (n, y) :: rest ->
      if x = n then y :: successori x rest else successori x rest

let rec vicini x = function
  | [] ->
      []
  | (n, y) :: rest ->
      if x = n then y :: vicini x rest
      else if x = y then n :: vicini x rest
      else vicini x rest

(** Algoritmi di base dei grafi **)

(** Visita di un grafo in profondità (grafo orientato) **)
(* depth_first_collect: 'a graph -> 'a -> 'a list *)
(* depth_first_collect grafo x = lista dei nodi raggiungibili da x in grafo *)

let depth_first_collect g start =
  (* search: 'a list -> 'a list -> 'a list *)
  (* search visited pending = nodi raggiungibili da qualche nodo in pending 
   * mediante un cammino che non passa per i nodi già visitati @ visited *)
  let rec search visited = function
    | [] ->
        visited
    | x :: rest ->
        if List.mem x visited then search visited rest
          (* si usa vicini per grafo non orientato *)
        else search (x :: visited) (successori x g @ rest)
  in
  search [] [start]

(* visita in profondità per verificare se tutti i nodi raggiungibili da start 
 * soddisfano un predicato p *)

(* depth_first_all: 'a graph -> 'a -> ('a -> bool) -> bool *)
(* depth_first_all graph start p = true se tutti i nodi raggiungibili da start
 * soddisfano il predicato p, false altrimenti *)
let depth_first_all g start p =
  let rec search visited = function
    | [] ->
        true
    | x :: rest ->
        if List.mem x visited then search visited rest
        else p x && search (x :: visited) (successori x g @ rest)
  in
  search [] [start]

(* breadth_first_collect: 'a graph -> 'a -> 'a list *)
(* breadth_first_collect g start = lista dei nodi raggiungibili da start in g *)
(* search: 'a list -> 'a list -> 'a list *)
(* search visited pending = nodi raggiungibili da qualche nodo in pending 
 * mediante un cammino che non passa per visited @ visited *)

(** Visita in ampiezza di un grafo **)
let rec breadth_first_collect g start =
  let rec search visited = function
    | [] ->
        visited
    | x :: rest ->
        if List.mem x visited then search visited rest
          (* vicini se il grafo non è orientato *)
        else search (x :: visited) (rest @ successori x g)
  in
  search [] [start]

(* cercare se dal nodo start è raggiungibile un nodo che soddisfa un predicato *)
(* search_node: 'a graph -> 'a -> ('a -> bool) -> 'a *)
(* search_node g start p = un nodo raggiungibile da start nel grafo g che 
 * soddisfa p *)

let search_node g start p =
  let rec search visited = function
    | [] ->
        raise Not_found
    | x :: rest ->
        if List.mem x visited then search visited rest
        else if p x then x (* vicini se il grafo non è orientato *)
        else search (x :: visited) (successori x g @ rest)
  in
  search [] [start]

(* ricerca di un cammino mediante backtracking *)
(* search_path: 'a graph -> 'a -> ('a -> bool) -> 'a list *)
(* search_path g x p = una lista che rappresenta il cammino in un grafo 
 * da start fino a un nodo raggiungibile che soddisfa p *)

let search_path g start p =
  let rec from_node visited a =
    if List.mem a visited then raise Not_found
    else if p a then [a]
    else a :: from_list (a :: visited) (vicini a g)
  and from_list visited = function
    | [] ->
        raise Not_found
    | a :: rest -> (
      try from_node visited a with _ -> from_list visited rest )
  in
  from_node [] start
