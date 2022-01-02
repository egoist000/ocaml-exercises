type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* path: ('a -> bool) -> 'a tree -> 'a list *)
(* path p tree = riporta un cammino dalla radice a una foglia t contenente 
 * tutti nodi che non soddisfano il predicato p, solleva un eccezione se tale 
 * cammino non esiste *)

let rec path p = function
  | Empty ->
      []
  | Tr (x, Empty, Empty) ->
      if p x then failwith "Errore" else [x]
  | Tr (x, l, r) ->
      if p x then failwith "Errore" else x :: (try path p l with _ -> path p r)
