(* Group the elements of a set into disjoint subsets. (medium) *)

(*
 * group ["a"; "b"; "c"; "d"] [2; 1];;
 * - : string list list list =
 * [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 *  [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 *  [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 *  [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
 *)

(* TODO *)

let group lst sizes = 
    let init = List.map(function size -> size, []) sizes

