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

(* foglia_costo: 'int ntree -> (int * int) *)
(* foglia_costo tree = restituisce la coppia etichetta, foglia più costosa 
 * dell'albero *)

let max_coppia c1 c2 = if snd c1 > snd c2 then c1 else c2

let foglia_costo t =
  let rec aux costo = function
    | Tr (x, []) ->
        (x, costo + x)
    | Tr (x, subtrees) ->
        costo_altri (x, x + costo) subtrees
  (* costo_altri: (int * int) 'a ntree list *)
  (* costo_altri c treelst = la coppia di costo massimo tra la radice e i 
   * sottoalberi in treelst *)
  and costo_altri c = function
    | [] ->
        c
    | t1 :: trest ->
        max_coppia (aux (snd c) t1) (costo_altri c trest)
  in
  aux 0 t

(* foglia_costo t = (20, 65) *)
