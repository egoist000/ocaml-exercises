(* trips: 'a list -> ('a * 'a * 'a) list *)
(* trips lst = una lista contenente tutte le triple adiacenti di lst *)

let rec trips = function
    | x::y::z::rest ->
            (x, y, z)::trips (y::z::rest)
    | _ -> []
