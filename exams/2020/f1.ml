(* Definisco un tipo chiamato metro *)
type metro = (int * int * string) list

let m =
  [ (1, 2, "A")
  ; (2, 3, "A")
  ; (3, 1, "A")
  ; (2, 4, "B")
  ; (4, 5, "B")
  ; (4, 6, "C")
  ; (6, 3, "C")
  ; (5, 7, "D")
  ; (6, 7, "D") ]

(**    1    **)

(* line: metro -> string -> int list *)
(* line m ln = una lista che rappresenta le stazioni della metro collegate
 * alla linea ln senza ripetizioni *)

let rec line m ln =
  match m with
  | [] ->
      []
  | (s1, s2, linea) :: rest ->
      let res = line rest ln in
      if ln = linea then
        s1 :: s2 :: List.filter (function e -> e <> s1 && e <> s2) res
      else res

(**    2    **)

(* vicini: int -> metro -> (int * string) list *)
(* vicini stazione m = una lista di coppie (s, ln) tale che s è una stazione 
 * direttamente collegata a stazione mediante la linea ln *)

let rec vicini stazione = function
  | [] ->
      []
  | (s1, s2, linea) :: rest ->
      if s1 = stazione then (s2, linea) :: vicini stazione rest
      else if s2 = stazione then (s1, linea) :: vicini stazione rest
      else vicini stazione rest

(**    3    **)

(* raggiungi: metro -> int -> int -> int -> int list *)
(* raggiungi m maxc start goal = una lista rappresentante un percorso da start 
 * a goal dove si cambia stazione al più maxc volte *)

let raggiungi m maxc start goal =
  let rec from_node cambi curr_ln visited s =
    if List.mem s visited then raise Not_found
    else if cambi > maxc then raise Not_found
    else if s = goal then [s]
    else s :: from_list cambi curr_ln (s :: visited) (vicini s m)
  and from_list cambi curr_ln v = function
    | [] ->
            raise Not_found
    | (s, line) :: rest -> (
        if line <> curr_ln then
          try from_node (cambi + 1) line v s
          with Not_found -> from_list cambi curr_ln v rest
        else
          try from_node cambi curr_ln v s
          with Not_found -> from_list cambi curr_ln v rest )
  in
  from_node (-1) "" [] start
