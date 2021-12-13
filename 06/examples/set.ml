(* powerset: 'a list -> 'a list list *)
(* powerset set = una lista contenente tutti i sottoinsiemi di set *)

let rec powerset = function
    | [] -> [[]]
    | x::rest ->
            powerset rest @ List.map (List.cons x) (powerset rest);;

(* prodotto cartesiano *)
(* cartprod: 'a list -> 'b list -> ('a * 'b) list *)

let rec pair x y = (x, y)

let rec cartprod set1 set2 =
    match set1 with
    | [] -> []
    | x::rest ->
            (List.map (pair x) set2) @ cartprod rest set2
