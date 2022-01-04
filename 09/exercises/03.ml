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

(* foglie_in_lista: 'a list -> 'a ntree -> bool *)
(* foglie_in_lista lst t = true se tutte le foglie di t sono presenti in lst
 * false altrimenti *)

let rec foglie_in_lista lst = function
  | Tr (x, []) ->
      List.mem x lst
  | Tr (x, subtrees) ->
      verifica lst subtrees

(* verifica 'a list -> 'a ntree list -> bool *)
(* verifica lst subtrees = true se tutte le foglie del sottoalbero sono in lst
 * false altrimenti *)
and verifica lst = function
  | [] ->
      true
  | t1 :: trest ->
      foglie_in_lista lst t1 && verifica lst trest
