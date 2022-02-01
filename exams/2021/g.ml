(* rappresento un grafo orientato mediante lista di archi *)

type 'a graph = ('a * 'a) list

let successori x g = List.map snd (List.filter (function c, _ -> c = x) g)

(* depth_limited 'a graph -> 'a -> 'a -> int -> 'a list *)
(* depth_limited grafo start goal depth = un cammino da start a goal in un 
 * grafo orientato tale che la sua lunghezza non sia superiore a depth *)

let depth_limited grafo start goal depth =
  let rec from_node curr_depth node visited =
    if List.mem node visited then raise Not_found
    else if node = goal && curr_depth <= depth then [node]
    else
      node
      :: from_list (curr_depth + 1) (node :: visited) (successori node grafo)
  and from_list d v = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try from_node d x v with Not_found -> from_list d v rest )
  in
  from_node 0 start []

(**    2    **)

(* path: 'a graph -> 'a -> 'a -> int -> 'a list *)
(* path grafo start goal max_depth = ricerca per approfondimenti successivi
 * di un cammino da start a goal *)

(* per approfondimenti successivi si intende la ricerca di un cammino a partire
 * da depth = 0, se la ricerca fallisce, si incrementa depth di 1, fino al 
 * raggiungimento di max_depth o di un cammino valido *)

let path grafo start goal max_depth =
  let rec aux iter =
    if iter > max_depth then raise Not_found
    else
      try depth_limited grafo start goal iter with Not_found -> aux (iter + 1)
  in
  aux 0

let grafo_prova =
  [ (1, 2)
  ; (1, 4)
  ; (2, 4)
  ; (4, 4)
  ; (4, 3)
  ; (3, 2)
  ; (4, 5)
  ; (5, 6)
  ; (6, 3)
  ; (3, 6) ]

(*
 * # depth_limited grafo_prova 1 6 2;;
 * Exception: Not_found.
 *)

(*
 * # depth_limited grafo_prova 1 6 3;;
 * - : int list = [1; 4; 3; 6]
 *)

(*
 * # depth_limited grafo_prova 1 6 6;;
 * - : int list = [1; 2; 4; 3; 6]
 *)

(*
 * # path grafo_prova 1 6 3;;
 * - : int list = [1; 4; 3; 6]
 *)
