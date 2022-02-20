(* 木構造を表す型 *)
type tree_t = 
    | Empty
    | Leaf of int
    | Node of tree_t * int * tree_t

