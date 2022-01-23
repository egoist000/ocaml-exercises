(* Rappresentazione di un albero binario *)
type 'a tree = Empty | Tr of ('a * 'a tree * 'a tree)

let albero =
  Tr
    ( 1
    , Tr
        ( 2
        , Tr (4, Tr (5, Empty, Empty), Tr (6, Empty, Empty))
        , Tr (4, Tr (7, Empty, Empty), Empty) )
    , Tr
        ( 3
        , Tr (8, Tr (9, Empty, Empty), Tr (10, Empty, Empty))
        , Tr (2, Tr (4, Tr (3, Empty, Empty), Empty), Tr (12, Empty, Empty)) )
    )

let rec labels = function
  | Empty ->
      []
  | Tr (x, l, r) ->
      (x :: labels l) @ labels r

let discendenti n tree =
  let rec aux = function
    | Empty ->
        []
    | Tr (x, l, r) ->
        if x = n then labels l @ labels r else aux l @ aux r
  in
  let res = aux tree in
  if res <> [] then n :: res else res

(** test **)
(*
 * # discendenti 4 albero;;
 * - : int list = [4; 5; 6; 7; 3]
 *)

(*
 * # discendenti 2 albero;;
 * - : int list = [2; 4; 5; 6; 4; 7; 4; 3; 12]
 *)
