(* 関数と初期値とリストが与えられたとき左から順にfを施しこむ関数 *)
(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f init lst = match lst with 
    [] -> init
    | first :: rest -> fold_left f (f init first) rest

let sum_list lst = 
    let rec hojo lst total = match lst with 
    [] -> []
    | first :: rest -> first + total :: hojo rest (first + total)
    in hojo lst 0

(* 整数のリストを受け取りそれまでの数の合計からなるリストを返す *)
(* TODO : fold_leftでリファクタ *)
(* sum_list : int list -> int list *)
let hojo lst x = match lst with 
    [] -> [x]
    | first :: rest -> x + first :: lst
(* sum_list : int list -> int list *)
let sum_list lst = fold_left hojo [] lst

let test1 = sum_list [] = []
let test2 = sum_list [4] = [4]
let test3 = sum_list [1; 3] = [1; 4]
let test4 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]

let test1 = sum_list [] 
let test2 = sum_list [4]
let test3 = sum_list [1; 3] 
let test4 = sum_list [3; 2; 1; 4] 
