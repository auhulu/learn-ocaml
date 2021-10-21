(* 駅名の情報を格納するレコード型 *)
type ekimei_t = { kanji : string ; kana : string ; romaji : string ; shozoku : string }

(* 駅間の情報を格納するレコード型 *)
type ekikan_t = { kiten : string ; shuten : string ; keiyu : string ; kyori : float ; jikan : int}

(* 駅名の情報を表示する関数 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei = match ekimei with 
    { kanji = a ; kana = b ; romaji = c ; shozoku = d } ->
        d ^ "、 " ^ a ^ "(" ^ b ^ ")"

let nakayama = { kanji = "東京" ; kana = "とうきょう" ; romaji = "tokyo" ; shozoku = "山手線" }
