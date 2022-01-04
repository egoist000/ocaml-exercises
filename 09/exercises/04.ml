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

(* numero_di_foglie: 'a ntree -> int *)
(* numero_di_foglie ntree = il numero di foglie di un albero n-ario *)

let rec numero_di_foglie = function
  | Tr (_, []) ->
      1
  | Tr (_, subtrees) ->
      conta subtrees

(* conta: 'a ntree list -> int *)
(* conta ntree_lst = il numero delle foglie dei sottoalberi della radice
 * della chiamata ricorsiva *)
and conta = function
  | [] ->
      0
  | t1 :: trest ->
      numero_di_foglie t1 + conta trest
