(* Tipo induttivo per definire le espressioni *)

type expr =
    | Int of int
    | Var of string
    | Sum of expr * expr
    | Diff of expr * expr
    | Mult of expr * expr
    | Div of expr * expr

(* Problema: valutare un espressione in un ambiente *)

type ambiente = (string * int) list

(* eval: ambiente -> expr -> int *)

(*
 * eval env e = valore dell’espressione e nell’ambiente env. Errore se qualche
 * variabile in e non ha un valore associato in env.
 *)

let rec eval env = function
    | Int n -> n
    | Var x ->
            (try List.assoc x env
            with Not_found -> failwith "eval")
    | Sum (e1, e2) ->
            (eval env e1) + (eval env e2)
    | Diff (e1, e2) ->
            (eval env e1) - (eval env e2)
    | Mult (e1, e2) ->
            (eval env e1) * (eval env e2)
    | Div (e1, e2) ->
            (eval env e1) / (eval env e2)

(* Rappresentazione alternative delle expressioni *)

type op = Sum | Diff | Mult | Div

type expr2 = 
    | Int of int
    | Var of string
    | Apply of op * expr * expr

let op2mat = function
    | Sum -> (+)
    | Diff -> (-)
    | Mult -> ( * )
    | Div -> (/)

let rec eval2 env = function
    | Int n -> n
    | Var x -> List.assoc x env
    | Apply (op, e1, e2) ->
            op2mat op (eval env e1) (eval env e2)
 
