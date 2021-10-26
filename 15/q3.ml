(* 2以上n以下の自然数のリストを受け取りその範囲の素数をリスト化して返す *)
(* sieve : int list -> int list *)
let rec sieve lst = match lst with
    [] -> [] 
    | first :: rest -> first :: (sieve (List.filter (fun x -> not (x mod first = 0)) rest))

(* 2以上n以下の自然数のリストを生成 *)
let rec enumerate n = 
    let rec f i = 
    if i > n then []
    else i :: f(i + 1)
    in f 2

(* 整数を受け取りそれ以下の素数をリスト化して返す *)
(* prime : int -> int list  *)
let prime n = sieve (enumerate n)

let test1 = prime 2 = [2]
let test2 = prime 1 = []
let test3 = prime 20 = [2; 3; 5; 7; 11; 13; 17; 19]