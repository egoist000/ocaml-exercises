(* Definisco un albero n-ario *)

type 'a ntree = Tr of ('a * 'a ntree list)

let albero =
  Tr
    ( 1
    , [ Tr (2, [Tr (3, []); Tr (4, []); Tr (2, [])])
      ; Tr (5, [Tr (11, []); Tr (10, [])])
      ; Tr (3, [Tr (9, []); Tr (7, []); Tr (10, [])]) ] )


(* pesi: 'a ntree -> 'a list *)
(* pesi ntree = una lista che rappresenta i pesi di tutti i rami che vanno dalla
 * radice ad una foglia *)

let pesi tree = 
    let rec aux acc = function
        | Tr(l, []) -> [l + acc]
        | Tr(n, subtrees) ->
                List.flatten(List.map (aux (n + acc)) subtrees)
    in aux 0 tree
