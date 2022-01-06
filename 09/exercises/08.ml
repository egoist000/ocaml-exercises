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

(* ramo_da_lista: 'a ntree -> 'a list -> 'a -> 'a list *)
(* ramo_da_lista tree lst k = un ramo di tree dalla radice a una foglia 
 * etichettata da k che passi per tutti gli elementi di lst una volta e contenga
 * solo nodi etichettati da elementi di lst *)

let rec remove k = function
  | [] ->
      raise Not_found
  | x :: rest ->
      if x = k then rest else x :: remove k rest

let rec ramo_da_lista t lst k =
  match t with
  | Tr (x, []) ->
          if x = k && lst = [x] then [x] else failwith "path"
  | Tr (x, subtrees) ->
      let newlst = try remove x lst with Not_found -> failwith "path" in
      x::iter newlst k subtrees

and iter lst k = function
  | [] ->
          failwith "path"
  | t1 :: trest ->
          (try ramo_da_lista t1 lst k with _ -> iter lst k trest)
