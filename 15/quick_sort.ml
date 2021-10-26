(* クイックソートのための補助関数 *)
(* lstの中からnより小さい要素を取り出す *)
(* take_less : int -> int list -> int list *)
let take_less n lst = List.filter (fun x -> x <= n) lst

(* lstの中からnより大きい要素を取り出す *)
(* take_greater : int -> int list -> int list *)
let take_greater n lst = List.filter (fun x -> x > n) lst

(* 受け取ったリストをクイックソート(昇順)する *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst = match lst with
    [] -> []
    | first :: rest -> quick_sort (take_less first rest) @ [first] @ quick_sort (take_greater first rest)

let test1 = quick_sort [] = []
let test2 = quick_sort [1] = [1]
let test3 = quick_sort [1; 2] = [1; 2]
let test4 = quick_sort [2; 1] = [1; 2]
let test5 = quick_sort [3; 5; 8; 2] = [2; 3; 5; 8]

(* q15用にコードも修正済み *)
let test6 = quick_sort [2; 1; 2] = [1; 2; 2]