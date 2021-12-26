type expr =
  | Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

(* Un Jolly è un carattere speciale '@' che può comparire in un espressione *)
(* Una espressione è una expr che non contiene Jolly *)
(* Un modello è un espressione che contiene uno o più Jolly *)

let rec pattern_matching e m =
  match e, m with
  | _, Jolly ->
      true
  | Var s1, Var s2 -> s1 = s2
  | Int i1, Int i2 -> i1 = i2
  | Sum (e1, e2), Sum (m1, m2) ->
      pattern_matching e1 m1 && pattern_matching e2 m2
  | Diff (e1, e2), Diff (m1, m2) ->
      pattern_matching e1 m1 && pattern_matching e2 m2
  | Mult (e1, e2), Mult (m1, m2) ->
      pattern_matching e1 m1 && pattern_matching e2 m2
  | Div (e1, e2), Div (m1, m2) ->
      pattern_matching e1 m1 && pattern_matching e2 m2
  | _ ->
      false

let espressione = Sum(Var "a", Diff(Mult(Var "b", Var "c"), Var "d"))
