type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* segui_bool: bool list -> 'a tree -> 'a *)
(* segui_bool blst tree = la radice del sottoalbero di tree che si ottiene 
 * seguendo per ogni elemento di blst il sottoalbero di sinistra se il valore 
 * è true, il sottoalbero di destra se il valore p false *)

let rec segui_bool blst tree =
    match blst, tree with
    | (_, Empty) -> failwith "Error"
    | ([], Tr(x, _, _)) -> x
    | (b::brest, Tr(_, l, r)) ->
            if b then segui_bool brest l
            else segui_bool brest r
