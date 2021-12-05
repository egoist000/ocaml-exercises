exception BadOperation

(* somma_ore: (int * int) -> (int * int) -> int * int *)
(* sommma_ore h1 h2 = la somma della rappresentazione delle due ore. 
 * un eccezione se h1 o h2 non rappresentano un ora corretta *)

let somma_ore (h1, m1) (h2, m2) =
    let h = h1 + h2
    in let m = m1 + m2
    in if m > 59 && h > 23 then raise BadOperation
    else ((h + (m / 60)) mod 24, m mod 60)
