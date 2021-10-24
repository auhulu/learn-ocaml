(* 目的 : 関数を2つ受け取りその関数を合成した関数を返す *)
(* compose : ('b -> 'c) -> ('a -> 'b) -> a -> c *)
let compose f g = let com x = f (g x) in com

let time2 x = x * 2
let add3 x = x + 3

let test1 = compose time2 add3 4 = 14
let test2 = compose time2 add3 2 = 10