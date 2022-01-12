type 'a graph = ('a * 'a) list

let is_prime n =
  if n <= 3 then n > 1
  else if n mod 2 = 0 || n mod 3 = 0 then false
  else
    let rec aux i =
      if i * i >= n then true
      else if n mod i = 0 || n mod (i + 2) = 0 then false
      else aux (i + 6)
    in
    aux 5

let successori x g = List.map snd (List.filter (function c, _ -> c = x) g)

(* cammino_di_primi: int graph -> int -> int -> int list *)
(* cammino_di_primi g start end = un cammino da start a end in cui tutti i nodi
 * sono primi *)

let cammino_di_primi g start goal =
  let rec from_node a visited =
    if List.mem a visited then raise Not_found
    else if is_prime a then
      if a = goal then [goal]
      else a :: from_list (a :: visited) (successori a g)
    else raise Not_found
  and from_list visited = function
    | [] ->
        raise Not_found
    | n :: nrest -> (
      try from_node n visited with _ -> from_list visited nrest )
  in
  from_node start []
