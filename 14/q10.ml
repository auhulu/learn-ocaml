(* 受け取ったリストの要素の和を返す *)
(* sum : int list -> int *)
let sum lst = List.fold_right (fun a b -> a + b) lst 0

let test1 = sum [] = 0
let test2 = sum [3; 5; 7] = 15

(* 受け取ったリストの要素の数を返す *)
(* sum : 'a list -> int *)
let length lst = List.fold_right(fun a b -> b + 1) lst 0

let test3 = length [] = 0
let test4 = length ["a"; "b"] = 2

(* 2つのリストを結合する *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 = List.fold_right (fun first rest -> first :: rest) lst1 lst2

let test5 = append [1; 2] [] = [1; 2]
let test6 = append [1; 2] [3; 4] = [1; 2; 3; 4]