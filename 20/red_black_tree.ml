type color_t = Red | Black

type ('a, 'b) rb_tree_t = Empty |  Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a ,'b) rb_tree_t

(* 目的；赤黒木を受け取り整形したものを返す *)
(* balance: rb_tree_t -> rb_tree_t *)
let balance rb_tree = match rb_tree with
    Node(Node(Node(a,x1,x2,Red,b),y1,y2,Red,c),z1,z2,Black,d)
  | Node(Node(a,x1,x2,Red,Node(b,y1,y2,Red,c)),z1,z2,Black,d)
  | Node(a,x1,x2,Black,Node(Node(b,y1,y2,Red,c),z1,z2,Red,d))
  | Node(a,x1,x2,Black,Node(b,y1,y2,Red,Node(c,z1,z2,Red,d)))
      -> Node(Node(a,x1,x2,Black,b),y1,y2,Red,Node(c,z1,z2,Black,d))
  | _ -> rb_tree

(* 目的：赤黒木とkeyとvalueを受け取りそれを挿入した木を返す *)
(* insert: rb_tree_t -> 'a -> 'b -> rb_tree_t *)
let rec insert tree k v = 
    let rec tmp tree = match tree with 
        Empty -> Node(Empty, k, v, Red, Empty)
        | Node(left, key, value, color, right) ->
            if k = key then Node(left, key, v, color, right)
            else if k < key then balance(Node(tmp left, key, value, color, right))
            else balance(Node(left, key, value, color, tmp right))
    in match tmp tree with
        Empty -> assert false
        | Node(left, key, value, color, right) -> Node(left, key, value, Black, right)

(* 目的：treeの中にキーkに対応するvalueを返す *)
(* 見つからなければNot_foundを起こす *)
(* search: rb_tree_t -> 'a -> 'b *)
let rec search tree k = match tree with
    Empty -> raise Not_found
    | Node(left, key, value, color, right) -> 
        if k = key then value
        else if k < key then search left k
        else search right k


(* balanceテスト *) 
let rb_tree1 = 
  Node (Node (Node (Empty, 10, "x", Red, Empty), 13, "y", Red, Empty), 
	15, "z", Black, Empty) 
let rb_tree2 = 
  Node (Node (Empty, 10, "x", Red, Node (Empty, 13, "y", Red, Empty)), 
	15, "z", Black, Empty) 
let rb_tree3 = 
  Node (Empty, 10, "x", Black, 
	Node (Node (Empty, 13, "y", Red, Empty), 15, "z", Red, Empty)) 
let rb_tree4 = 
  Node (Empty, 10, "x", Black, 
	Node (Empty, 13, "y", Red, Node (Empty, 15, "z", Red, Empty))) 
let rb_tree5 = 
  Node (Node (Empty, 10, "x", Black, Empty), 13, "y", Red, 
	Node (Empty, 15, "z", Black, Empty)) 
let rb_tree6 = Empty 
let test1 = balance rb_tree1 = rb_tree5 
let test2 = balance rb_tree2 = rb_tree5 
let test3 = balance rb_tree3 = rb_tree5 
let test4 = balance rb_tree4 = rb_tree5 
let test5 = balance rb_tree6 = rb_tree6 

(* insertテスト *)
let rb_tree0 = Empty 
let rb_tree1 = insert rb_tree0 10 "x" 
let rb_tree2 = insert rb_tree1 13 "y" 
let rb_tree3 = insert rb_tree2 15 "z" 
 
let test1 = rb_tree1 = Node (Empty, 10, "x", Black, Empty) 
let test2 = rb_tree2 = Node (Empty, 10, "x", Black, 
			     Node (Empty, 13, "y", Red, Empty)) 
let test3 = rb_tree3 = Node (Node (Empty, 10, "x", Black, Empty), 
			     13, "y", Black, 
			     Node (Empty, 15, "z", Black, Empty)) 

(* searchテスト *) 
let rb_tree = 
  Node (Node (Empty, 10, "x", Black, Empty), 13, "y", Red, 
	Node (Empty, 15, "z", Black, Empty)) 
let test1 = search rb_tree 10 = "x" 
let test2 = search rb_tree 13 = "y" 
let test3 = search rb_tree 15 = "z" 
(* Not_found を起こすので実行後コメントアウト *) 
(* let test4 = search rb_tree 17  *)