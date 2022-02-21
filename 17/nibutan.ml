(* 木構造を表す型 *)
type tree_t = 
    | Empty
    | Leaf of int
    | Node of tree_t * int * tree_t

(* 目的：dataが二分探索木treeに含まれるかを調べる *)
(* search: tree_t -> int -> bool *)
let rec search tree data = match tree with
    | Empty -> false
    | Leaf(n) -> data = n
    | Node(t1, n ,t2) -> 
        if data = n then true
        else if data < n then search t1 data
        else search t2 data

(* 目的；二分探索木treeにdataを追加したtreeを返す *)
(* 既に存在する場合は追加しない *)
(* insert: tree_t -> int -> tree_t *)
let rec insert tree data = match tree with
    | Empty -> Leaf(data)
    | Leaf(n) ->
        if data = n then Leaf(n)
        else if data < n then Node(Leaf(data), n, Empty)
        else Node(Empty, n, Leaf(data))
    | Node(t1, n, t2) ->
        if data = 0 then Node(t1, n, t2)
        else if data < n then Node(insert t1 data,n,t2)
        else Node(t1, n, insert t2 data)

(* 木の例 *)
let tree1 = Empty
let tree2 = Leaf(3)
let tree3 = Node(Leaf(1), 2, Leaf(3))
let tree4 = Node(Empty, 7, Leaf(9))
let tree5 = Node(tree3, 6, tree4)

(* テスト *)
let test1 = search tree1 3 = false
let test2 = search tree2 3 = true
let test3 = search tree2 4 = false
let test4 = search tree5 6 = true
let test5 = search tree5 2 = true
let test6 = search tree5 1 = true
let test7 = search tree5 4 = false

let test1 = insert Empty 3 = Leaf(3)
let test2 = insert (Leaf(3)) 2 = Node(Leaf(2), 3, Empty)
let test3 = insert (Leaf(3)) 3= Leaf(3)
let test4 = insert (Leaf(3)) 4 = Node(Empty, 3, Leaf(4))
let test5 = insert tree5 4 = Node(Node(Leaf(1), 2, Node(Empty, 3, Leaf(4))),6, Node(Empty, 7, Leaf(9)))