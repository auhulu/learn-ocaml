let twice f = let g x = f (f x) in g

let tt = twice twice twice