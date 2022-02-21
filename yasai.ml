(* itemと八百屋リストを受け取りその値段を返す *)
(* price: string -> (string * int) list -> int option *)
let rec price item yaoya_list = match yaoya_list with
    [] -> None
    | (yasai, nedan) :: rest ->
        if item = yasai then Some(nedan)
        else price item rest

(* オプション型を使った例外処理の例 *)
(* 目的：yasai_listを買った時の値段の合計を調べる *)
(* total_price: string ;ist -> (string * int) list -> int *)
let total_price yasai_list yaoya_list = 
    (* 目的：yasai_listを買った時の値段の合計を調べる(オプション型で返す) *)
    (* hojo: string list -> int option *)
    let rec hojo yasai_list = match yasai_list with
        [] -> Some(0)
        | first :: rest -> match price first yaoya_list with
            None -> None
            | Some(p) -> match hojo rest with
                None -> None
                | Some(q) -> Some(p + q)
    in match hojo yasai_list with
        None -> 0
        | Some(p) -> p

(* 以下例外処理の構文でリファクタ *)
exception Urikire

(* itemと八百屋リストを受け取りその値段を返す *)
(* 見つからない場合、Urikireという例外を返す *)
(* price: string -> (string * int) list -> int *)
let rec price item yaoya_list = match yaoya_list with
    [] -> raise Urikire
    | (yasai, nedan) :: rest ->
        if item = yasai then nedan
        else price item rest

(* 目的：yasai_listを買った時の値段の合計を調べる *)
(* total_price: string ;ist -> (string * int) list -> int *)
let total_price yasai_list yaoya_list = 
    (* 目的：yasai_listを買った時の値段の合計を調べる *)
    (* hojo: string list -> int *)
    let rec hojo yasai_list = match yasai_list with
        [] -> 0
        | first :: rest -> price first yaoya_list + hojo rest
    in try hojo yasai_list with Urikire -> 0



(* 八百屋においてある野菜と値段のリスト *)
let yaoya_list = [
  ("トマト", 300);
  ("たまねぎ", 200);
  ("にんじん", 150);
  ("ほうれん草", 200)
]

(* total_priceのテスト *)
let test1 = total_price ["トマト"; "にんじん"] yaoya_list = 450
let test2= total_price ["きゅうり";"にんじん"] yaoya_list = 0