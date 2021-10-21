(* 目的 : 2つのリストを受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let rec append lst1 lst2 = match lst1 with
    [] -> lst2
    | first :: rest ->
        first :: append rest lst2

let test1 = append [] [] = []
let test2 = append [] [1; 2] = [1; 2]
let test3 = append [1; 2] [3; 4] = [1; 2; 3; 4]