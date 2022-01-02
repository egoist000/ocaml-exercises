type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let a1 = Tr(1, Tr(2, Empty, Empty), Tr(2, Empty, Empty))
let a2 = Tr(10, Tr(20, Empty, Empty), Tr(20, Empty, Empty))
let a3 = Tr(10, Tr(20, Empty, Empty), Tr(30, Empty, Empty))

(* stessa_struttura: 'a tree 'a tree -> bool *)
let rec stessa_struttura t1 t2 = 
    match t1, t2 with 
    | Empty, Empty -> true
    | Tr(_,_,_), Empty | Empty, Tr(_, _, _) -> false
    | Tr(_, l1, r1), Tr(_, l2, r2) ->
            stessa_struttura l1 l2 && stessa_struttura r1 r2

(* esiste_mapping: 'a tree -> 'a tree -> bool *)

let rec costruisci_mapping t1 t2 =
  match (t1, t2) with
  | Empty, Tr (_, _, _) | Tr (_, _, _), Empty | Empty, Empty ->
      []
  | Tr (x, l1, r1), Tr (y, l2, r2) ->
      ((x, y) :: costruisci_mapping l1 l2) @ costruisci_mapping r1 r2

let esiste_mapping t1 t2 =
  let mapping = costruisci_mapping t1 t2 in
  let rec verifica = function
    | [] ->
        true
    | (x, y) :: rest ->
        List.for_all (function k, v -> x <> k || y = v) rest && verifica rest
  in
  verifica mapping
