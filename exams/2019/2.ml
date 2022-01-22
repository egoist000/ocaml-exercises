type color = Rosso | Verde | Neutro

type 'a graph = ('a * 'a) list

let rec vicini x = function
  | [] ->
      []
  | (c1, c2) :: rest ->
      if c1 = x then c1 :: vicini x rest
      else if c2 = x then c2 :: vicini x rest
      else vicini x rest

let rec remove x = function
  | [] ->
      []
  | n :: rest ->
      if x = n then rest else n :: remove x rest

(* path: 'a graph -> ('a * color) list -> color list -> 'a -> 'a list *)
(* path grafo cols clrs start = un cammino aciclico da start che contenga almeno 
 * tutti i colori in cols *)

let path g cols clrs start =
  let rec from_node lst visited a =
    if List.mem a visited then raise Not_found
    else if List.assoc cols = a then [a]
    else if clrs = [] then []
    else a :: from_list (a :: visited) lst (vicini a g)
  and from_list visited lst = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try from_node (remove x lst) visited x
      with Not_found -> from_list visited lst rest )
  in
  from_node [] [] start
