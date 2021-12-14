(* Flatten a nested list structure. (medium) *)

type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let flatten lst =
    let rec aux tmp = function
        | [] -> tmp
        | One x::rest -> aux (x::tmp) rest
        | Many lst::rest -> aux (aux tmp lst) rest
    in List.rev (aux [] lst)


(* # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
 * - : string list = ["a"; "b"; "c"; "d"; "e"] *)
