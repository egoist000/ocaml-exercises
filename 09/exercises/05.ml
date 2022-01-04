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

(* lista_guida: 'a list -> 'a ntree -> 'a *)
(* lista_guida lst ntree = la radice dell'albero che si ottiene seguendo 
 * gli elementi di lst *)

let rec lista_guida lst ntree =
  match (lst, ntree) with
  | [], Tr (x, _) ->
      x
  | x :: rest, Tr (_, subtrees) ->
      lista_guida rest (try List.nth subtrees x with _ -> failwith "Error")
