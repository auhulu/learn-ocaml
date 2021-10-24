(* initからはじめてlstの要素を右から順にfを実行する *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init = match lst with
    [] -> init
    | first :: rest -> f first (fold_right f rest init)

(* 受け取ったリストの要素の和を返す *)
(* sum : int list -> int *)
let sum lst = let add a b = a + b in fold_right add lst 0

let test1 = sum [] = 0
let test2 = sum [3; 5; 7] = 15

(* 受け取ったリストの要素の数を返す *)
(* sum : 'a list -> int *)
let length lst = let add1 a b = b + 1 in fold_right add1 lst 0

let test3 = length [] = 0
let test4 = length ["a"; "b"] = 2

(* 1つの要素をリストの先頭に加える *)
(* cons : 'a -> 'a list -> 'a list *)
let cons first rest = first :: rest

(* 2つのリストを結合する *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 = fold_right cons lst1 lst2

let test5 = append [1; 2] [] = [1; 2]
let test6 = append [1; 2] [3; 4] = [1; 2; 3; 4]