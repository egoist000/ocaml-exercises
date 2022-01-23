let rec remove e = function
  | [] ->
      raise Not_found
  | x :: rest ->
      if x = e then rest else x :: remove e rest

(* complemento: 'a list -> 'a list -> 'a list *)
(* complemento superset set = il complemento di set rispetto a superset *)

let rec complemento superset = function
  | [] ->
      superset
  | e :: rest ->
      complemento (remove e superset) rest

(* # complemento [1;2;3;4;5;6] [2;4;6];;
 * - : int list = [1; 3; 5] *)

(* # complemento [1;2;3;4;5;6] [2;7;6];;
 * Exception: Not_found.*)
