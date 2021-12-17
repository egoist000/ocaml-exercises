(* Decode a run-length encoded list. (medium) *)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let rec upto e = function
    | 0 -> []
    | n -> e::upto e (n - 1)

let rec decode = function
    | [] -> []
    | x::rest ->
            match x with
            | One e -> e::decode rest
            | Many (c, k) -> (upto k c) @ decode rest

