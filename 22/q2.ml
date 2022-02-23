(* 配列を与えたらその長さのフィボナッチ数列を格納した配列にして返す *)
(* fib_array: int array -> int array *)
let rec fib_array array =
    let len = Array.length array in
    let rec fib i = 
    if i < len then (
        if i = 0 then array.(i) <- 0
        else if i = 1 then array.(i) <- 1
        else array.(i) <- array.(i - 1) + array.(i - 2);
        fib (i + 1)
    ) else () in (fib 0; array) 


(* テスト *)
let test1 = fib_array [|0|] = [|0|]
let test2 = fib_array [|0;0|] = [|0;1|]
let test3 = fib_array [|0;0;0;0|] = [|0;1;1;2|]
let test4 = fib_array [|0;0;0;0;0;0;0;0;0;0|] = [|0;1;1;2;3;5;8;13;21;34|]