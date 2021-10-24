(* nから1までのリストを作る *)
(* enumerate : int -> int list *)
let rec enumerate n =
    if n = 0 then [] else n :: enumerate(n - 1)

(* nの約数のリストを作る *)
(* divisor : int -> int list *)
let divisor n = List.filter(fun x -> n mod x = 0) (enumerate n)

(* m以下の完全数のリストを返す *)
(* perfect : int -> int list *)
let perfect m =
    List.filter (fun n -> List.fold_right (+) (divisor n) 0 - n = n ) (enumerate m)

let test = perfect 10000 = [8128; 496; 28; 6]