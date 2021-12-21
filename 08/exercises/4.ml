type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* num_foglie: 'a tree -> int *)
(* num_foglie t = numero di foglie di un albero binario *)

let rec num_foglie = function
    | Empty -> 0
    | Tr(_, Empty, Empty) -> 1
    | Tr(_, l, r) ->
            (num_foglie l) + (num_foglie r)
