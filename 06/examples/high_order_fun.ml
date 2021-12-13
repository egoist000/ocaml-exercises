(* sum: (int -> int) -> int -> int -> int *)
(* sum fun lower upper = sommatoria tra lower e upper a cui viene applicata 
 * una funzione per ogni elemento *)

let rec sum f lower upper =
    if lower > upper then 0
    else f lower + sum f (lower + 1) upper

let square x = x * x

(* List.sort è un esempio di funzione di ordine superiore *)
(* List.sort: ('a -> 'a -> int) -> 'a list -> 'a list *)

(* cmp 'a * 'b -> 'c * 'b -> int *)
(* cmp (_, x) (_, y) = 1 se x è minore di y *)

let cmp (_, x) (_, y) = - (compare x y)

(* pairsort: ('a * 'b) list -> ('a * 'b) list *)
(* pairsort lst = una lista ordinata in modo decrescente secondo il secondo 
 * elemento della coppia *)

let pairsort lst = List.sort cmp lst
