(* obiettivo: leggere da file una sequenza di numeri interi, 
 * scrivere il numero di interi letti, la loro somma e la media *)

(* media: string -> unit *)
(* media s = legge i numeri dal file s *)

let media file =
    (* apro il canale di input *)
    let inputchan = open_in file
    in let rec loop () =
        try
            let n = int_of_string(input_line inputchan) (* lettura file *)
            in let (tot, somma) = loop ()
            in (tot + 1, somma + n)
        with _ -> close_in inputchan; (0, 0)
    in let (n, somma) = loop ()
    in print_string ("Letti "^(string_of_int n)^
                     " interi\nSomma: "^(string_of_int somma)^
                     "\nMedia: "^
                     (string_of_float ((float_of_int somma)/.(float_of_int n)))
                     ^"\n")
