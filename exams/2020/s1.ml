(**    1    **)

(* remove: 'a -> 'a list -> 'a list *)
(* remove e lst = una lista ottenuta da lst eliminando tutte le occorrenze di 
 * e in lst *)

(* versione ricorsiva *)

let rec remove e = function
  | [] ->
      []
  | x :: rest ->
      if x = e then remove e rest else x :: remove e rest

(* versione iterativa o ricorsiva di coda *)

let remove_it e lst =
  let rec loop tmp = function
    | [] ->
        tmp
    | x :: rest ->
        if x = e then loop tmp rest else loop (x :: tmp) rest
  in
  List.rev (loop [] lst)

(**    2    **)

(* Le versioni iterative, rispetto alle versioni ricorsive sono più efficienti
 * dal punto di vista dell'utilizzo dello spazio di memoria nello stack 
 * utilizzato per le chiamate ricorsive, la versione ricorsiva utilizzerà più
 * chiamate e quindi più memoria rispetto alla versione iterativa, questa una volta
 * raggiunto il caso base non deve far altro che ritornare il valore costruito nelle
 * chiamate precedenti *)

(**    3    **)

(* rappresento un grafo mediante lista di archi *)

type 'a graph = ('a * 'a) list

let successori n g = List.map snd (List.filter (function c, _ -> c = n) g)

(* percorso: 'a graph -> 'a -> 'a -> 'a -> 'a list *)
(* percorso g start tappa target = una lista rappresentante un cammino da start 
 * a target che passi per il nodo tappa. Riporta un eccezione se tale cammino
 * non esiste *)

let percorso g start tappa target =
  let rec from_node visited a =
    if List.mem a visited then raise Not_found
    else if a = target && (List.mem tappa visited || a = tappa) then [a]
    else a :: from_list (a :: visited) (successori a g)
  and from_list v = function
    | [] ->
        raise Not_found
    | x :: rest -> (
      try from_node v x with Not_found -> from_list v rest )
  in
  from_node [] start

let grafo =
  [(1, 3); (2, 6); (3, 4); (3, 5); (3, 6); (4, 2); (4, 5); (5, 4); (6, 5)]
