(* 目的 : リストの中の最小値を返す *)
(* minimum : int list -> int *)
let rec minimum first lst = match lst with
    [] -> first
    | second :: rest -> 
        let min_rest = minimum second rest in 
        if first < min_rest then first else min_rest

let test = minimum 3 [2; 6; 4; 1; 8] = 1