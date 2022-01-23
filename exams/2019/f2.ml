type color = Rosso | Verde | Neutro

type 'a graph = ('a * 'a) list

let grafo =
  [ (1, 2)
  ; (1, 3)
  ; (3, 2)
  ; (2, 5)
  ; (3, 5)
  ; (3, 4)
  ; (4, 5)
  ; (5, 6)
  ; (5, 7)
  ; (6, 7)
  ; (7, 8) ]

let colassoc =
  [ (1, Neutro)
  ; (2, Rosso)
  ; (3, Verde)
  ; (4, Verde)
  ; (5, Neutro)
  ; (6, Verde)
  ; (7, Rosso)
  ; (8, Neutro) ]

let rec vicini x = function
  | [] ->
      []
  | (c1, c2) :: rest ->
      if c1 = x then c2 :: vicini x rest
      else if c2 = x then c1 :: vicini x rest
      else vicini x rest

let rec remove x = function
  | [] ->
      []
  | n :: rest ->
      if x = n then rest else n :: remove x rest

let get_color x assoc_lst = try List.assoc x assoc_lst with _ -> Neutro

(* path: 'a graph -> ('a * color) list -> color list -> 'a -> 'a list *)
(* path grafo cols clrs start = un cammino aciclico da start che contenga almeno 
 * tutti i colori in cols *)

let path g cols clrs start =
  let rec from_node lst visited a =
    if List.mem a visited then raise Not_found
    else if lst = [] then [a]
    else a :: from_list (a :: visited) lst (vicini a g)
  and from_list visited lst = function
    | [] -> []
    | x :: rest -> (
      try from_node (remove (get_color x cols) lst) visited x
      with Not_found -> from_list visited lst rest )
  in
  if List.length clrs <= 1 then [] else from_node clrs [] start
