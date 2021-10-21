(* 目的 : 自然数を受け取り、0から受け取った自然数までの2乗の和を返す *)
(* sum_of_square : int -> int *)
let rec sum_of_square n = 
    if n = 0 then 0
    else n * n + sum_of_square (n - 1)

let test = sum_of_square 4 = 30