(* Problema: date le ultime X estrazioni del superenalotto 
 * (sequenze di 6 numeri comprese tra 1 e 90) determinare i 6 numeri 
 * che più probabilmente usciranno nella prossima estrazione *)

(* test assumendo che i numeri estratti siano da 1 a 10 e che ogni 
 * estrazione dia 3 numeri *)

let estrazioni = 
  [[1; 7; 3]; [5; 4; 8]; [8; 7; 1]; [6; 10; 3]; [4; 2; 3]; [1; 5; 6];
   [8; 3; 3]; [7; 7; 2]; [2; 10; 8]; [3; 5; 6]; [4; 9; 7]; [1; 6; 3];
   [8; 4; 6]; [6; 3; 3]; [5; 6; 8]; [6; 7; 1]; [9; 5; 8]; [8; 1; 2];
   [10; 7; 1]; [7; 4; 6]];;

let flatten_estra =
  [1; 7; 3; 5; 4; 8; 8; 7; 1; 6; 10; 3; 4; 2; 3; 1; 5; 6; 8; 3; 3; 
   7; 7; 2; 2; 10; 8; 3; 5; 6; 4; 9; 7; 1; 6; 3; 8; 4; 6; 6; 3; 3; 
   5; 6; 8; 6; 7; 1; 9; 5; 8; 8; 1; 2; 10; 7; 1; 7; 4; 6]

(* upto: int -> int -> int list *)
(* upto n m = una lista contenente i numeri da n a m compresi *)

let rec upto n m =
    if n > m then []
    else n::upto(n + 1) m

(* conta: 'a -> ''a list -> int *)
(* conta n lista = numero di occorrenze di n in lista *)

let rec conta n = function
    | [] -> 0
    | x::rest -> 
            if n = x then 1 + conta n rest
            else conta n rest

(* conta_tutti 'a list -> a' list -> 'a * int *)
(* conta_tutti tutti lista = conta ogni elemento di tutti in in lista *)

let rec conta_tutti tutti lista = 
    match tutti with
    | [] -> []
    | x::rest -> (x, conta x lista)::conta_tutti rest lista

(* flatten 'a list list -> 'a list *)
(* flatten lista = la lista "schiacciata" *)

let rec flatten = function
    | [] -> []
    | lst::rest -> lst @ flatten rest

(* minore: 'a * 'b -> 'c * 'b -> bool *)
(* minore (_, x) (_, y) = true se x è minore di y false altrimenti *)

let minore (_, x) (_, y) = x < y

(* split: 'a list -> 'a list * 'a list *)
(* split lst = ritorna una coppia di liste di grandezza pressappoco uguale 
 * nella quale sono suddivisi gli elementi della lista *)

let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x::y::rest -> 
            let (xs, ys) = split rest
            in (x::xs, y::ys)

(* merge: 'a list -> 'a list -> 'a list *)
(* merge xs ys = fusione di due liste ordinate *)

let rec merge xs ys =
    match (xs, ys) with
    | ([], _) -> ys
    | (_, []) -> xs
    | x::xs, y::ys -> 
            if minore x y then x::merge xs (y::ys)
            else y::merge (x::xs) ys

let rec sort = function
    | [] -> []
    | [x] -> [x]
    | lst  -> 
            let (xs, ys) = split lst
            in merge (sort xs) (sort ys)

let compare_pairs (_, x) (_, y) = compare x y

(* take: int -> 'a list -> 'a list *)
(* take n list = i primi n elementi della lista list *)

let rec take n = function
    | [] -> []
    | x::rest -> 
            if n <= 0 then []
            else x::take (n - 1) rest

(* primi: (a' * b') list -> a' list *)
(* primi lst = presa ula lista di coppie ritorna una lista contenente solo 
 * il primo elemento delle suddette *)

let rec primi = function
    | [] -> []
    | (x, _)::rest -> x::primi rest

(* Problema principale da risolvere *)
(* super: int list list -> int -> int -> int list *)
(* super estrazioni dim higer = riporta una sequenza di grandezza dim, 
 * contenente i numeri da 1 a higher che nella lista delle estrazioni 
 * compaiono meno volte *)

let super estrazioni dim higher =
    primi(take dim 
         (sort 
            (conta_tutti (upto 1 higher) (flatten estrazioni))))
