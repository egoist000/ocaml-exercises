(* Rappresentiamo insiemi finiti mediante liste *)

(* mem 'a -> a' list -> bool *)
(* mem x lst = true se lst contiene x *)

let rec mem x = function
    | [] -> false
    | y::rest -> x = y || mem x rest

(* union 'a list -> 'a list -> 'a list *)
(* union a b = l'unione degli insiemi A e B *)

let rec union a b =
    match a with
    | [] -> b
    | x::rest -> 
            if mem x b then union rest b
            else x::union rest b

let rec intersect a b =
    match a with
    | [] -> []
    | x::rest -> 
            if mem x b then x::intersect rest b
            else intersect rest b

let diff a b =
    let i = intersect a b
    in let rec loop = function
        | [] -> []
        | x::rest ->
                if not(mem x i && mem x b) then x::loop rest
                else loop rest
    in loop a
            
