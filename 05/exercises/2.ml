(* setunion: 'a list -> 'a list -> 'a list *)
(* setunion set1 set2 = unione insiemistica tra i due insiemi *)

let rec setunion set = function
    | [] -> set
    | x::rest ->
            if List.mem x set then setunion set rest
            else setunion (x::set) rest
(* setintersect: 'a list -> 'a list -> 'a list *)
(* setintersect set1 set2 = intersezione insiemistica tra i sue insiemi *)
let rec setintersect set = function
    | [] -> []
    | x::rest ->
            if List.mem x set then x::setintersect set rest
            else setintersect set rest
(* setdiff: 'a list -> 'a list -> 'a list *)
(* setdiff set1 set2 = differenza insiemistica tra i due insiemi *)
let rec setdiff set1 set2 =
    match set1 with
    | [] -> []
    | x::rest ->
            if List.mem x set2 then setdiff rest set2
            else x::setdiff rest set2

(* subset: 'a list -> 'a list -> bool *)
(* subset set1 set2 = true se set1 è sottoinsieme di set2 false altrimenti *)

let rec subset set1 set2 =
    match set1 with
    | [] -> true
    | x::rest -> List.mem x set2 && subset rest set2

