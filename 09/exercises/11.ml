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

(* same_structure: 'a ntree -> 'b ntree -> bool *)
(* same_structure t1 t2 = true se i due alberi hanno la stessa struttura, 
 * false altrimenti *)

let rec same_structure t1 t2 =
  match (t1, t2) with
  | Tr (_, []), Tr (_, _ :: _) | Tr (_, _ :: _), Tr (_, []) ->
      false
  | Tr (_, s1), Tr (_, s2) ->
      iter s1 s2

(* iter 'a ntree list -> 'b ntree list -> bool *)
(* iter s1 s2 = true se tutti i sottoalberi di s1 e s2 nella chiamata ricorsiva
 * hanno la stessa struttura, false in tutti gli altri casi *)
and iter s1 s2 =
  match (s1, s2) with
  | [], [] ->
      true
  | t1 :: t1rest, t2 :: t2rest ->
      same_structure t1 t2 && iter t1rest t2rest
  | _, _ ->
      false
