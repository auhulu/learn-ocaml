(* フィボナッチ数を求める関数 *)
let rec fib n = if n < 2 then n else fib(n - 1) + fib(n - 2)

(* フィボナッチ数を再帰回数とともに求める *)
(* 呼び出し時には引数としてカウンタ0を入力 *)
(* fib: int -> int -> (int * int) *)
let rec fib n c =
    let c0 = c + 1 in 
    if n < 2  then (n, c0)
    else let (r1, c1) = fib(n - 1) c0 in
    let (r2, c2) = fib (n - 2) c1 in
    (r1 + r2, c2)

(* 参照型を使用して再帰回数を求める *)
(* fib: int -> int *)
let count = ref 0
let rec fib n = (count := !count + 1; if n < 2 then n else fib(n - 1) + fib(n - 2))