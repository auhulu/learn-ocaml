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