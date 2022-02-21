module Tree = struct
(* 二分探索木を表す型、よくわからん *)
type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a ,'b) t

(* 空の木 *)
let empty = Empty

(* 目的：treeにキーがkで値がvを挿入した木を返す *)
(* insert: ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
let rec insert tree k v = match tree with
    Empty -> Node(Empty, k, v, Empty)
    | Node(left, key, value, right) ->
        if k = key then Node(left, key, v, right)
        else if k < key then Node(insert left k v, key, value, right)
        else Node(left, key, value, insert right k v)

(* 目的：treeの中にキーkに対応するvalueを返す *)
(* 見つからなければNot_foundを起こす *)
(* search: ('a, 'b) t -> 'a -> 'b *)
let rec search tree k = match tree with
    Empty -> raise Not_found
    | Node(left, key, value, right) -> 
        if k = key then value
        else if k < key then search left k
        else search right k
end

(* 二分探索木を表すモジュールのシグネイチャ *)
module type Tree_t = sig
type ('a, 'b) t
(* キーが'a型で値が'b型の木 *)

val empty : ('a, 'b) t
(* 使い方 Empty *)
(* 空の木 *)

val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
(* 使い方 insert tree key value *)
(* 目的：treeにキーがkで値がvを挿入した木を返す *)

val search : ('a, 'b) t -> 'a -> 'b 
(* 使い方 search tree key *)
(* 目的：treeの中にキーkに対応するvalueを返す *)
(* 見つからなければNot_foundを起こす *)
end

(* シグネイチャを用いた宣言との違い *)
(* Tree.insert Tree.empty "a" 3 *)