type color_t = Red | Black

type ('a, 'b) rb_tree_t = Empty |  Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a ,'b) rb_tree_t

(* 空の木 *)
let empty = Empty

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