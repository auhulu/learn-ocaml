(* 2つの整数(大きい順)を受け取りユークリッド互除法によって最大公約数を返す *)
(* gcd : int -> int -> int *)
let rec gcd m n = 
    if n = 0 then m
    else gcd n (m mod n)

let test1 = gcd 12 9 = 3
let test2 = gcd 24 16 = 8