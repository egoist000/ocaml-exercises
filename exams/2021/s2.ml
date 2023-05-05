(* definisco un tipo per rappresentare alberi n-ari *)

type 'a ntree = Tr of ('a * 'a ntree list)

(* cerca_foglia ('a * int) list -> 'a ntree -> 'a *)
(* cerca_foglia guida t = una foglia che si ottiene da t scegliendo, per ogni
 * nodo intermedio x, l'n-esimo sottoalbero associato al valore di x in guida *)

let rec cerca_foglia guida = function
  | Tr (leaf, []) ->
      leaf
  | Tr (x, subtrees) ->
      let value = try List.assoc x guida with Not_found -> failwith "guida" in
      itera_subtrees value guida subtrees

and itera_subtrees v g = function
  | [] ->
      raise Not_found
  | x :: rest ->
      if v = 1 then cerca_foglia g x else itera_subtrees (v - 1) g rest

let guida_prova = [(1, 3); (2, 2); (3, 1); (10, 1); (16, 2); (11, 2)]

let albero =
  Tr
    ( 1
    , [ Tr
          ( 2
          , [Tr (3, [Tr (4, []); Tr (5, [])]); Tr (6, [Tr (7, [])]); Tr (8, [])]
          )
      ; Tr (9, [])
      ; Tr
          ( 10
          , [ Tr (11, [Tr (12, []); Tr (13, []); Tr (14, [])])
            ; Tr (15, [])
            ; Tr (16, [Tr (17, []); Tr (18, [Tr (19, []); Tr (20, [])])]) ] ) ]
    )

(*
 * # cerca_foglia guida_prova albero;;
 * - : int = 13
 cerca_foglia [(1,3);(2,2);(3,1);(16,2);(11,2)] albero;;
Exception: Failure "guida".*)

(*
 * cerca_foglia [(1,3);(2,2);(3,1);(16,2);(11,2)] albero;;
 * Exception: Failure "guida".
 *)
