type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let albero1 =
  Tr
    ( "A"
    , Tr ("B", Tr ("C", Empty, Empty), Tr ("D", Empty, Empty))
    , Tr ("E", Empty, Tr ("F", Empty, Empty)) )

let albero2 =
  Tr
    ( "A"
    , Tr ("B", Tr ("G", Empty, Empty), Empty)
    , Tr ("H", Empty, Tr ("F", Empty, Empty)) )

(* max_common_subtree: string tree -> string tree -> string tree *)
(* max_common_subtree t1 t2 = il massimo sottoalbero comune a t1 e t2 
 * partendo dalla radice, se il nodo x di t1 è diverso dal corrispondente di t2
 * il nodo corrispondente a x nel sottoalbero comune sarà etichettato da una
 * foglia @ *)

let rec max_common_subtree t1 t2 =
  match (t1, t2) with
  | Empty, Tr _ | Tr _, Empty -> 
          Tr ("@", Empty, Empty)
  | Tr (x, l1, r1), Tr (y, l2, r2) ->
      if x <> y then Tr ("@", Empty, Empty)
      else Tr (x, max_common_subtree l1 l2, max_common_subtree r1 r2)
  | _ -> Empty
