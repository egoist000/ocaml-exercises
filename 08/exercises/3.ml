type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* Albero di esempio *)

let albero = Tr("A", Tr("H", Tr("X", Tr("Y", Empty, Empty), Empty), Empty),
                Tr("O", Tr("I", Empty, Tr("M", Empty, Tr("T", Empty, Empty))), 
                Empty))

(* 'a list -> 'a tree -> bool *)
(* foglie_in_lista lst tree = true se tutte le foglie in tree sono in lst *)

let rec foglie_in_lista lst = function
    | Empty -> true
    | Tr(x, Empty, Empty) -> true
    | Tr(_, l, r) ->
            (foglie_in_lista lst l) && (foglie_in_lista lst r)
