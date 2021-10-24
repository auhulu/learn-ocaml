(* 整数のリストを受け取り偶数のみ残したものを返す *)
(* even : int list -> int list *)
let even = let f n = n mod 2 = 0 in List.filter f 

let test1 = even [] = []
let test2 = even [0] = [0]
let test3 = even [1] = []
let test4 = even [2] = [2]
let test5 = even [3; 4; 9] = [4]
let test6 = even [2; 3; 7; 8] = [2; 8]