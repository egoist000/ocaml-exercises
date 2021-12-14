(* definizione di labirinto le caselle sono rappresentate come una coppia 
 * (r, c) riga, colonna nella matrice di dimensione n *)

let labirinto = (5,
    [((1,0),"oro"); ((3,1),"oro"); ((4,3),"oro");
    ((0,1),"argento"); ((2,4),"argento"); ((0,2),"mostro");
    ((1,1),"mostro"); ((1,3),"mostro"); ((2,3),"mostro");
    ((3,0),"mostro"); ((4,2),"mostro")])

let in_riga (_, lst) riga value =
    List.exists (function (c, v) ->
        (riga = fst c) && (value = v)) lst

let trova_colonna (_, lst) riga value =
    let elem = List.find (function (c, v) ->
        (riga = fst c) && (value = v)) lst
    in snd(fst elem)

let rec upto n m =
    if n > m then []
    else n::upto (n + 1) m

let rec in_tutte mat v = 
    let (dim, _) = mat in
    List.for_all (function r ->
        in_riga mat r v) (upto 0 (dim - 1))
