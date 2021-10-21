(* 目的 : 2つのリストを受け取り長さが同じかどうかのbool値を返す *)
(* equal_length : 'a list -> 'a list -> bool *)
let rec equal_length lst1 lst2 = match (lst1, lst2) with
([], []) -> true
| (first1 :: rest1, []) -> false
| ([], first2 :: rest2) -> false
| (first1 :: rest1, first2 :: rest2) -> equal_length rest1 rest2


let test1 = equal_length [] [] = true
let test2 = equal_length [] [1; 2] = false
let test3 = equal_length [1; 4] [2; 3] = true