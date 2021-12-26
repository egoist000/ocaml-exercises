type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let albero =
  Tr
    ( 0
    , Tr (10, Tr (2, Empty, Empty), Tr (5, Empty, Empty))
    , Tr
        ( 6
        , Tr (6, Tr (3, Empty, Empty), Empty)
        , Tr (4, Tr (3, Empty, Empty), Tr (4, Empty, Empty)) ) )

(* foglie_costi: int tree -> (int * int) list *)
(* foglie_costi t = riporta una lista di coppie (f, n) fove f è l'etichetta
 * della foglia in t e n il suo costo *)

let foglie_costi t =
  let rec aux sum = function
    | Empty ->
        []
    | Tr (x, Empty, Empty) ->
        [(x, x + sum)]
    | Tr (x, l, r) ->
        let new_sum = x + sum in
        aux new_sum l @ aux new_sum r
  in
  aux 0 t
