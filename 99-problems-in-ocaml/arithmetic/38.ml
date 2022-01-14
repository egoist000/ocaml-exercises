let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let coprime n1 n2 = gcd n1 n2 = 1

let phi m =
  let rec aux c =
    if c > m then 0 else if coprime c m then 1 + aux (c + 1) else aux (c + 1)
  in
    aux 1

let factors n =
  let rec aux c n =
    if n = 1 then []
    else if n mod c = 0 then
      match aux c (n / c) with
      | (h, m) :: rest when h = c ->
          (h, m + 1) :: rest
      | l ->
          (c, 1) :: l
    else aux (c + 1) n
  in
  aux 2 n

let rec pow b e = if e = 0 then 1 else b * pow b (e - 1)

let phi_improved n =
  let lst = factors n in
  let rec aux = function
    | [] ->
        1
    | (p, m) :: rest ->
        (p - 1) * pow p (m - 1) * aux rest
  in
  aux lst

(* Compare the two methods of calculating Euler's totient function. (easy) *)

let benchmark n =
    let t1 = Sys.time ()
    in let res1 = phi n 
    in let f1 = Sys.time () -. t1
    in let t2 = Sys.time ()
    in let res2 = phi_improved n
    in let f2 = Sys.time () -. t2
    in let () = Printf.printf "standard phi function execution time: %fs\n" (f1)
    in let () = Printf.printf "improved phi function execution time: %fs\n" (f2)
    in (res1, res2)
