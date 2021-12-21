type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let albero =
  Tr
    ( 0,
      Tr (10, Tr (2, Empty, Empty), Tr (5, Empty, Empty)),
      Tr
        ( 6,
          Tr (6, Tr (3, Empty, Empty), Empty),
          Tr (4, Tr (3, Empty, Empty), Tr (4, Empty, Empty)) ) )

let foglia_costo tree =
  let rec aux tot = function
    | Empty -> failwith "No_Foglia"
    | Tr (x, Empty, Empty) -> (x, tot + x)
    | Tr (x, l, Empty) -> aux (tot + x) l
    | Tr (x, Empty, r) -> aux (tot + x) r
    | Tr (x, l, r) ->
        let cl, tl = aux (tot + x) l in
        let cr, tr = aux (tot + x) r in
        if tl > tr then (cl, tl) else (cr, tr)
  in
  aux 0 tree
