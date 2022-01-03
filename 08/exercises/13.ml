type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* path_coprente: 'a tree -> 'a list -> 'a list *)
(* path_coprente t nodes = una lista contenente il cammino dalla radice a una
 * foglia contenente solo nodi appartenenti a nodes *)

let rec remove x = function
  | [] ->
      []
  | y :: rest ->
      if x = y then rest else y :: remove x rest

let rec path_coprente t lst =
  match t with
  | Empty ->
      failwith "Errore"
  | Tr (x, Empty, Empty) ->
      if lst = [] || lst = [x] then [x] else failwith "Errore"
  | Tr (x, l, r) ->
      let newl = remove x lst in
      x :: (try path_coprente l newl with _ -> x :: path_coprente r newl)
