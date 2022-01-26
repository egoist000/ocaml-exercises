(* filquad: int list -> int list -> int list *)
(* filquad ints quads = ritorna la lista contenente tutti gli elementi di 
 * ints il cui quadrato è presente in quads *)

(* ALGO RICORSIVO *)

let rec filquad_rec ints quads =
  match ints with
  | [] ->
      []
  | x :: rest ->
      if List.mem (x * x) quads then x :: filquad_rec rest quads
      else filquad_rec rest quads

(* ALGORITMO ITERATIVO *)

let filquad_it ints quads =
  (* aux: int list -> int list -> int list *)
  (* aux tmp ints = tutti gli elementi di ints il cui quadrato è presente in 
   * quads @ tmp *)
  let rec aux tmp = function
    | [] ->
        tmp
    | x :: rest ->
        if List.mem (x * x) quads then aux (x :: tmp) rest else aux tmp rest
  in
  List.rev (aux [] ints)

(* UTILIZZANDO LE FUNZIONI DI ORDINE SUPERIORE *)

let filquad ints quads =
  List.filter (function e -> List.mem (e * e) quads) ints
