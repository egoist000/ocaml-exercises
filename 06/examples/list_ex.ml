(* MAP *)

(* inits: 'a list -> 'a list list *)
(* inits lst = lista con tutti gli elementi iniziali di lst *)
(* inits [1;2;3;4] = [[1];[1;2];[1;2;3];[1;2;3;4]]*)

let rec inits = function
    | [] -> []
    | x::rest ->
            [x]::List.map (List.cons x) (inits rest);;

(* ITER *)
let it lst = List.iter (function x -> print_endline (string_of_int x)) lst;;

(* FILTER *)

let rec my_filter p = function
    | [] -> []
    | x::rest ->
            if p x then x::my_filter p rest
            else my_filter p rest

let greater_than x y = y > x

let maggiori_di_cento lst = List.filter (greater_than 100) lst

(* FOR_ALL *)

let rec my_for_all p = function
    | [] -> true
    | x::rest -> p x && my_for_all p rest

let tutti_maggiori_di_100 lst = List.for_all (greater_than 100) lst


