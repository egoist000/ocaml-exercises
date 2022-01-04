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

(* visita in postordine di un albero n-ario *)
(* postorder: 'a ntree -> 'a list *)

let rec postorder = function
  | Tr (x, subtrees) ->
      List.flatten (List.map postorder subtrees) @ [x]

(* visita simmetrica *)
(* inorder: 'a ntree -> 'a list *)

let rec inorder = function
  | Tr (x, subtrees) -> (
    match subtrees with
    | [] ->
        [x]
    | t1 :: trest ->
        inorder t1 @ (x :: List.flatten (List.map inorder trest)) )
