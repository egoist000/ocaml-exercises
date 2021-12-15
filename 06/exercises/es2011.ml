(* find: 'a -> 'a list -> 'a list * 'a list *)
(* find x lst = find 3 [1;2;3;4;5;6;3] = ([1, 2], [4;5;6;3]) *)

let rec find x = function
    | [] -> failwith "Error"
    | k::rest ->
            if x = k then ([], rest)
            else let (b, a) = find x rest
            in (k::b, a)

(* spezza: 'a -> 'a list -> 'a list * 'a list *)
(* spezza x lst = (l1, l2) l1 tutti gli elementi di lst dalla prima alla 
 * seconda occorrenza di x, l2 tutti gli elementi che seguono *)

let rec spezza x = function
    | [] -> failwith "Error"
    | k::rest ->
            if k = x then
                let (_, l) = find k (k::rest)
                in find x l
            else spezza x rest
