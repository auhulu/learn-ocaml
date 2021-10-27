(* 整数のリストを受け取りそれまでの数の合計からなるリストを返す *)
(* sum_list : int list -> int list *)
let sum_list lst = 
    let rec hojo lst total = match lst with 
    [] -> []
    | first :: rest -> first + total :: hojo rest (first + total)
    in hojo lst 0

let test1 = sum_list [] = []
let test2 = sum_list [4] = [4]
let test3 = sum_list [1; 3] = [1; 4]
let test4 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]