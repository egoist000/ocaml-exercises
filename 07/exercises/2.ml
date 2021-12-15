(* definizione numero naturale *)

type nat = Zero | Succ of nat

(* somma : nat -> nat -> nat *)
let rec somma n m =
    match n with
    | Zero -> m
    | Succ k -> Succ(somma k m)

(* prodotto tra numeri naturali *)

let prodotto n m =
    let rec aux i acc =
        if i = m then acc
        else aux (Succ i) (somma n acc)
    in aux Zero Zero

