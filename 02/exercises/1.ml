(* ultime_cifre: int -> int * int *)
(* ultime_cifre num = riporta la coppia (x, y) rappresentante rispettivamente 
 * la penultima e l'ultima cifra dell'intero numerico n *)
let ultime_cifre num =
    (* n = il valore assoluto di num *)
    let n = abs num in
    ((n / 10) mod 10, n mod 10)

