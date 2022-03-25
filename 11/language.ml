type form =
  | True
  | False
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form
  | Imp of form * form

let f1 = Or (Not (Prop "p"), Prop "q")

let f2 = Not (Imp (Prop "p", Prop "q"))

let f3 = And (f1, f2)

let f4 = Or (f3, And (Prop "r", Prop "s"))

let f5 = Not (Imp (Not (Prop "p"), Prop "q"))

let f6 = Not (Or (Prop "p", Not (Prop "q")))

let f7 = Not (And (f5, f6))

let setadd x xs = if List.mem x xs then xs else x :: xs

let rec union lst ys =
  match lst with [] -> ys | x :: xs -> setadd x (union xs ys)

let rec atomlist = function
  | True | False ->
      []
  | Prop s ->
      [s]
  | Not f ->
      atomlist f
  | Or (f1, f2) ->
      union (atomlist f1) (atomlist f2)
  | And (f1, f2) ->
      union (atomlist f1) (atomlist f2)
  | Imp (f1, f2) ->
      union (atomlist f1) (atomlist f2)

(* f2s : form -> string *)
let rec f2s = function
  | True ->
      "T"
  | False ->
      "F"
  | Prop p ->
      p
  | Not f ->
      "-" ^ f2s f
  | And (f, g) ->
      String.concat "" ["("; f2s f; "&"; f2s g; ")"]
  | Or (f, g) ->
      String.concat "" ["("; f2s f; "|"; f2s g; ")"]
  | Imp (f, g) ->
      String.concat "" ["("; f2s f; "->"; f2s g; ")"]
