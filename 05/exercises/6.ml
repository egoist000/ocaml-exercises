let rec take n = function
    | [] -> []
    | x::rest ->
            if n <= 0 then []
            else x::take (n - 1) rest

let rec choose k lst =
    if List.length lst < k then []
    else (take k lst)::(choose k (List.tl lst))
