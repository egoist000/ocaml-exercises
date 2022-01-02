(* length sort *)

let length_sort lsts = List.sort List.compare_lengths lsts

(* length frequency sort *)

let comp (_, o1, _) (_, o2, _) = Int.compare o1 o2

let rec insertl (k, v) = function
  | [] ->
      [(k, 1, [v])]
  | (len, occ, lsts) :: rest ->
      if k = len then List.sort comp ((len, occ + 1, v :: lsts) :: rest)
      else List.sort comp ((len, occ, lsts) :: insertl (k, v) rest)

let map_len lst =
  let rec aux tmp = function
    | [] ->
        tmp
    | x :: rest ->
        aux (insertl (List.length x, x) tmp) rest
  in
  aux [] lst

let frequency_sort lst =
  let map = map_len lst in
  let rec aux = function [] -> [] | (_, _, lsts) :: rest -> lsts @ aux rest in
  aux map
