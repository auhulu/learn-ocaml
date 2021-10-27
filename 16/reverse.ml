(* reverseの補助 *)
(* hojo : a' list -> 'a list -> 'a list *)
let rec hojo lst tmp = match lst with
    [] -> tmp
    | first :: rest -> hojo rest (first :: tmp)

(* 与えられたリストを逆順で返す *)
(* reverse : 'a list -> 'a list *)
let reverse lst = hojo lst []

let test = reverse [1; 2; 3; 4; 5] 