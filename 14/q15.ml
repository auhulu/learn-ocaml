(* nから1までのリストを作る *)
(* enumerate : int -> int list *)
let rec enumerate n =
    if n = 0 then [] else n :: enumerate(n - 1)

(* 1からnまでの合計を求める *)
(* one_to_n : int list -> int *)
let one_to_n n =
    List.fold_right (+) (enumerate n) 0

let test = one_to_n 10 = 55