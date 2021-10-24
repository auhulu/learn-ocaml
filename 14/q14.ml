(* 文字列のリストを受け取り結合した文字列を返す *)
(* concat : string list -> string *)
let concat lst = List.fold_right(^) lst ""

let test = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"