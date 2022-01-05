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

(* tutte_foglie_costi: int ntree -> (int * int) list *)
(* tutte_foglie_costi ntree = la lista di coppie etichetta, costo nell'albero *)

let tutte_foglie_costi t =
  (* aux: int -> int ntree ->(int * int) list *)
  (* aux c tree = una lista di coppie etichetta, costo c + costo radice delle
   * foglie *)
  let rec aux c = function
    | Tr (x, []) ->
        [(x, x + c)]
    | Tr (x, subtrees) ->
        List.flatten (List.map (aux (x + c)) subtrees)
  in
  aux 0 t
