type color = Rosso | Verde | Neutro

(* conta_colori: ('a * color) list -> 'a list -> (color * int) list *)
(* conta_colori cols lst = una lista contenente tre coppie (Rosso, n) (Verde, m)
 * (k, Neutro) dove n rappresenta il numero dei colori rossi nella lista associativa,
 * lo stesso dicasi per m e k con i colori rispettivamente verde e 
 * non presenti nella lista *)

let colsp = [(2, Rosso); (3, Verde); (4, Verde); (6, Verde); (7, Rosso)]

let lstp = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let conta_colori cols lst =
  let rec aux n m k = function
    | [] ->
        [(Rosso, n); (Verde, m); (Neutro, k)]
    | x :: rest ->
        let col = try List.assoc x cols with _ -> Neutro in
        if col = Rosso then aux (n + 1) m k rest
        else if col = Verde then aux n (m + 1) k rest
        else aux n m (k + 1) rest
  in
  aux 0 0 0 lst
