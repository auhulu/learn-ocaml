(* 目的 : 昇順に並んでいる2つのリストをマージする *)
(* merge : int lst -> int lst2 -> int list *)
let rec merge lst1 lst2 = match (lst1, lst2) with
    ([], []) -> []
    | ([], first2 :: rest2) -> lst2
    | (first1 :: rest1, []) -> lst1
    | (first1 :: rest1, first2 :: rest2) -> 
        if first1 < first2 
        then first1 :: first2 :: merge rest1 rest2 
        else first2 :: first1 :: merge rest1 rest2


let test1 = merge [] [] = []
let test2 = merge [] [1; 2] = [1; 2]
let test3 = merge [1; 4] [2; 3] = [1; 2; 3; 4]