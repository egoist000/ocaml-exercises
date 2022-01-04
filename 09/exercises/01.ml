type 'a ntree = Tr of 'a * 'a ntree list

(*
 * Sia data la seguente definizione di tipo per la rappresentazoine di espres-
 * sioni come alberi n-ari:
 *)

type multi_expr =
  | MultiInt of int
  | MultiVar of string
  | MultiDiff of multi_expr * multi_expr
  | MultiDiv of multi_expr * multi_expr
  | MultiSum of multi_expr list
  | MultiMult of multi_expr list

(* subexpr: multi_expr -> multi_expr -> bool *)
(* multi_expr e1 e2 = true se e2 è una sottoespressione di e1, false altrimenti*)

let rec subexpr e1 e2 =
  e1 = e2
  ||
  match e1 with
  | MultiDiff (me1, me2) | MultiDiv (me1, me2) ->
      subexpr me1 e2 || subexpr me2 e2
  | MultiSum mlst | MultiMult mlst ->
      List.exists (function e -> subexpr e e2) mlst
  | _ ->
      false

(* subst: multi_expr -> string -> multi_expr -> multi_expr *)
(* subst expr v sost = un espressione che si ottiene da expr sostituendo con
 * sost ogni occorrenza della variabile v in expr *)

let rec subst expr v sost =
  match expr with
  | MultiVar s ->
      if v = s then sost else expr
  | MultiDiff (e1, e2) ->
      MultiDiff (subst e1 v sost, subst e2 v sost)
  | MultiDiv (e1, e2) ->
      MultiDiv (subst e1 v sost, subst e2 v sost)
  | MultiSum elist ->
      MultiSum (List.map (function e -> subst e v sost) elist)
  | MultiMult elist ->
      MultiMult (List.map (function e -> subst e v sost) elist)
  | _ ->
      expr
