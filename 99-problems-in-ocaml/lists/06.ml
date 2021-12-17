(* Find out whether a list is a palindrome. (easy) *)

let is_palindrome lst = List.rev lst = lst

(* # is_palindrome ["x"; "a"; "m"; "a"; "x"];;
 * - : bool = true
 * # not (is_palindrome ["a"; "b"]);;
 * - : bool = true *)
