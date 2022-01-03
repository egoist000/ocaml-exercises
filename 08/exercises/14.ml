type col = Rosso | Giallo | Verde | Blu

type 'a col_assoc = (col * 'a list) list

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let albero =
  Tr
    ( 1
    , Tr (2, Tr (8, Empty, Empty), Tr (3, Empty, Empty))
    , Tr
        ( 5
        , Tr (8, Tr (10, Empty, Empty), Tr (9, Empty, Empty))
        , Tr (7, Tr (4, Empty, Empty), Tr (8, Empty, Empty)) ) )

let provacol =
  [ (Rosso, [1; 2; 4; 7; 10])
  ; (Giallo, [3; 8; 11])
  ; (Verde, [0; 5; 6; 13])
  ; (Blu, [9; 12; 14; 15]) ]

(* colore: 'a -> 'a col_assoc -> col *)
(* colore x lst = il colore che contiene nella sua lista associativa il valore x
 * altrimenti solleva un'eccezione se tale colore non è presente nella lista 
 * associativa *)

let rec colore x = function
  | [] ->
      failwith "Errore"
  | (color, lst) :: rest ->
      if List.mem x lst then color else colore x rest

(* path_to: 'a -> 'a col_assoc -> 'a tree -> 'a list *)
(* path_to x clst t = una lista contenente il cammino dalla radice a una foglia
 * dove i colori associati ai nodi dell'albero sono diversi a due a due *)

let path_to x clst t =
  let rec aux prevc = function
    | Empty ->
        failwith "Errore"
    | Tr (y, Empty, Empty) ->
        if x = y && colore x clst <> prevc then [y] else failwith "Errore"
    | Tr (y, l, r) ->
        let thiscol = colore y clst in
        if thiscol = prevc then failwith "Errore"
        else y :: (try aux thiscol l with _ -> aux thiscol r)
  in
  match t with
  | Empty ->
      failwith "Errore"
  | Tr (y, Empty, Empty) ->
      if x = y then [y] else failwith "Errore"
  | Tr (y, l, r) ->
      let rootcol = colore y clst in
      y :: (try aux rootcol l with _ -> aux rootcol r)
