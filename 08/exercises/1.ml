
type expr =
    | Int of int
    | Var of string
    | Sum of expr * expr
    | Diff of expr * expr
    | Mult of expr * expr
    | Div of expr * expr

(*
 * Scrivere una funzione subexpr: expr -> expr -> bool che, date due
 * espressioni aritmetiche E1 e E2 determini se E2 è una sottoespressione di E1.
 *)

let rec subexpr e1 e2 =
    e1 = e2 ||
    match e1 with
    | Sum(x, y) | Diff(x, y) | Mult(x, y) | Div(x, y) ->
        subexpr x e2 || subexpr y e1
    | _ -> false

(* subst_in_expr: expr -> string -> expr -> expr *)
(* subst_in_expr e1 v e2 = date due espressioni e1 e e2 e il nome di una 
 * variabile, ritorna il valore di e1 che si ottiene sostituendo ogni occorenza
 * della variabile v con e2 *)

let rec subst_in_expr e1 v e2 =
    match e1 with
    | Int i -> Int i
    | Var e -> 
            if e = v then e2
            else e1
    | Sum(x, y) -> Sum(subst_in_expr x v e2, subst_in_expr y v e2)
    | Diff(x, y) -> Diff(subst_in_expr x v e2, subst_in_expr y v e2)
    | Mult(x, y) -> Mult(subst_in_expr x v e2, subst_in_expr y v e2)
    | Div(x, y) -> Div(subst_in_expr x v e2, subst_in_expr y v e2)


