type form =
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form

type 'a graph = ('a * 'a) list

let successori x g = List.map snd (List.filter (function c, _ -> c = x) g)

let contradictory lista = function
  | Not f ->
      List.mem (Not (Not f)) lista || List.mem f lista
  | f ->
      List.mem (Not f) lista

(* non_contradictory_path: form graph -> form -> form -> form list *)
(* non_contradictory_path g start end = una lista da start a end che riporta
 * se esiste un cammino non contraddittorio *)

let non_contradictory_path g start goal =
  let rec from_node a visited =
    if List.mem a visited || contradictory visited a then raise Not_found
    else if a = goal then [goal]
    else a :: from_list (a :: visited) (successori a g)
  and from_list visited = function
    | [] ->
        raise Not_found
    | n :: nrest -> (
      try from_node n visited with Not_found -> from_list visited nrest )
  in
  from_node start []
