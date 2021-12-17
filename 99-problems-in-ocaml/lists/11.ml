(* Modified run-length encoding. (easy) *)

(*
 * Modify the result of the previous problem in such a way that if an element 
 * has no duplicates it is simply copied into the result list. 
 * Only elements with duplicates are transferred as (N E) lists.
 *)

(*
 * Since OCaml lists are homogeneous, one needs to define a type to 
 * hold both single elements and sub-lists.
 *)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let mod_encode lst =
    let create_t count elem = 
        if count = 1 then One elem
        else Many (count, elem) in
    let rec aux c k = function
        | [] -> create_t c k::[]
        | x::rest ->
                if x = k then aux (c + 1) k rest
                else (create_t c k)::aux 1 x rest
    in try aux 0 (List.hd lst) lst
    with _ -> []


