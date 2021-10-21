(* 目的 :  リストと整数を受け取り昇順となる位置にその整数を挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> [n]
    | first :: rest 
        -> if first < n then first :: insert rest n else n :: lst

let test = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8]

(* 目的 : リストを受け取り昇順にソートしたリストを返す *)
(* insert_sort : list -> list *)
let rec insert_sort lst = match lst with 
    [] -> []
    | first :: rest
        -> insert (insert_sort rest) first

let test = insert_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8]
