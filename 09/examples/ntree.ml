(* Rappresentazione di un albero n-ario *)

type 'a ntree = Tr of 'a * 'a ntree list

let leaf x = Tr (x, [])

let t =
  Tr
    ( 1
    , [ Tr (2, [Tr (3, [leaf 4; leaf 5]); Tr (6, [leaf 7]); leaf 8])
      ; leaf 9
      ; Tr
          ( 10
          , [ Tr (11, [leaf 12; leaf 13; leaf 14])
            ; leaf 15
            ; Tr (16, [leaf 17; Tr (18, [leaf 19; leaf 20])]) ] ) ] )

let sumof = List.fold_left ( + ) 0

(* Calcolare dimensione di un albero n-ario *)
(* size: 'a ntre -> int *)
(* size t = numero di nodi di t *)

let rec size (Tr (_, tlst)) = 1 + sumof (List.map size tlst)

let rec size' (Tr (_, tlst)) = 1 + sumofsizes tlst

and sumofsizes = function [] -> 0 | t :: rest -> size t + sumofsizes rest
