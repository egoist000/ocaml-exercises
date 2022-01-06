type 'a ntree = Tr of 'a * 'a ntree list

(* path_non_pred: ('a -> bool ) -> 'a ntree -> 'a list *)
(* path_non_pred p tree = un cammino dalla radice a una foglia di tree 
 * che non contenga alcun nodo che soddisfa p, solleva un eccezione se tale 
 * cammino non esiste *)

let rec path_non_pred p = function
  | Tr (x, []) ->
      if p x then failwith "Errore" else [x]
  | Tr (x, subtrees) ->
      if p x then failwith "Errore"
      else x::iter p subtrees
  and iter p = function
      | [] -> failwith "Error"
      | t::trest ->
              try path_non_pred p t with _ -> iter p trest
