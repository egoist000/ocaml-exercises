(* definizione di un nuovo tipo, enumerato *)
type segnale = Linea | Punto 
               | Pausa | Errore (* per la codifica di caratteri 
                                   di cui non e` dato il codice *)
let morse =
  [ 'A', [Punto;Linea] ; (* non servono le parentesi per le coppie *)
    'B', [Linea;Punto;Punto;Punto];
    'C', [Linea;Punto;Linea;Punto];
    'D', [Linea;Punto;Punto];
    'E', [Punto] ;
    'F', [Punto;Punto;Linea;Punto];
    'G', [Linea;Linea;Punto];
    'H', [Punto;Punto;Punto;Punto];
    'I', [Punto;Punto];
    'J', [Punto;Linea;Linea;Linea];
    'K', [Linea;Punto;Linea];
    'L', [Punto;Linea;Punto;Punto];
    'M', [Linea;Linea];
    'N', [Linea;Punto];
    'O', [Linea;Linea;Linea];
    'P', [Punto;Linea;Linea;Punto];
    'Q', [Linea;Linea;Punto;Linea];
    'R', [Punto;Linea;Punto];
    'S', [Punto;Punto;Punto];
    'T', [Linea];
    'U', [Punto;Punto;Linea];
    'V', [Punto;Punto;Punto;Linea];
    'W', [Punto;Linea;Linea];
    'X', [Linea;Punto;Punto;Linea];
    'Y', [Linea;Punto;Linea;Linea];
    'Z', [Linea;Linea;Punto;Punto]
  ]

let encode = function
    | ' ' -> [Pausa]
    | c ->
            try List.assoc c morse
            with Not_found -> [Errore];;

let explode s = 
    let rec aux n res =
        if n < 0 then res
        else aux (n - 1) (s.[n]::res)
    in aux (String.length s - 1) [];;

let encode_msg msg = List.map encode (explode msg)
