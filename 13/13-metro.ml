#use "metro.ml"
(* ダイクストラ用の駅情報を格納するレコード型 *)
type eki_t = {namae: string; saitan_kyori: float; temae_list: string list}

(* 駅名の情報を表示する関数 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei = match ekimei with 
    { kanji = a ; kana = b ; romaji = c ; shozoku = d } ->
        d ^ "、 " ^ a ^ "(" ^ b ^ ")"

(* 目的 : ローマ字の駅名と駅名リストを受け取りその漢字表記を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with 
    [] -> ""
    | {kanji = a; kana = b; romaji = c; shozoku = d} :: rest ->
        if romaji = c then a else romaji_to_kanji romaji rest

(* 目的 : 漢字の駅名を2つと駅間リストを受け取り2駅間の距離を返す *)
(* 直接つながっていない場合はinfinityを返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori kanji1 kanji2 lst = match lst with
    [] -> infinity
    | {kiten = a; shuten = b; keiyu = c; kyori = d; jikan = e} :: rest ->
        if kanji1 = a && kanji2 = b then d
        else if kanji1 = b && kanji2 = a then d
        else get_ekikan_kyori kanji1 kanji2 rest

(* 目的 : ローマ字の駅名を2つ受け取り、直接つながっている場合は、繋がっていない場合は、を返す *)
(* 駅が存在しない場合はを返す *)
(* kyori_wo_hyoji : string -> string -> ekimei_t list -> ekikan_t list ->string *)
let kyori_wo_hyoji romaji1 romaji2 meilist kanlist = 
    let kanji1 = romaji_to_kanji romaji1 meilist in
    let kanji2 = romaji_to_kanji romaji2 meilist in
    if kanji1 = "" then romaji1 ^ "という駅は存在しません"
    else if kanji2 = "" then romaji2 ^ "という駅は存在しません"
    else let ekikan = get_ekikan_kyori kanji1 kanji2 kanlist in
        if ekikan = infinity then kanji1 ^ "駅と" ^ kanji2 ^ "駅はつながっていません"
        else kanji1 ^ "駅から" ^ kanji2 ^ "駅までは" ^ string_of_float ekikan ^ "kmです"

(* 目的 : ekimei_t型のリストを受け取りeki_t型のリストを返す *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let rec make_eki_list lst = match lst with
    [] -> []
    |{kanji = a; kana = b; romaji = c; shozoku = d} :: rest -> {namae = a; saitan_kyori = infinity; temae_list = []} :: make_eki_list rest

(* 目的 : eki_t型のリストと起点となる駅名(漢字)の文字列を受け取り、起点のみ距離0、temaeが始点の駅名のみからなるリストを返す *)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika lst kanji = match lst with 
    [] -> []
    | {namae = a; saitan_kyori = b; temae_list = c} as first :: rest ->
        if kanji = a then {namae = a; saitan_kyori = 0.0; temae_list = [a]} :: shokika rest kanji
        else first :: shokika rest kanji

(* 目的：昇順に並んでいる lst の正しい位置に ekimei を挿入する *) 
(* ekimei_insert : ekimei_t list -> ekimei_t -> ekimei_t list *) 
let rec ekimei_insert lst ekimei = match lst with
    [] -> [ekimei]
    | {kanji = a; kana = b; romaji = c; shozoku = d} as first :: rest  -> 
        match ekimei with {kanji = p; kana = q; romaji = r; shozoku = s} -> 
        if b = q then lst 
        else if b < q then first :: ekimei_insert rest ekimei
        else ekimei :: lst

(* 目的 : ekimei_t型のリストを受け取りひらがな順に整列し駅の重複を取り除いたekimei_t型のリストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec seiretsu lst = match lst with 
    [] -> []
    | first :: rest -> ekimei_insert (seiretsu rest) first 

(* 直前に確定した駅p(eki_t型)と未確定の駅q(eki_t型)を受け取り *)
(* 繋がっていたらqの最短距離と手前リストを更新してqを返す *)
(* 繋がっていなければqをそのまま返す *)
(* koushin1 : eki_t -> eki_t -> eki_t *)
let koushin1 p q = 
    let kyori = get_ekikan_kyori p.namae q.namae global_ekikan_list in
        if p.saitan_kyori +. kyori < q.saitan_kyori 
        then {namae = q.namae; saitan_kyori = p.saitan_kyori +. kyori; temae_list = q.namae :: p.temae_list}
        else q

(* 直前に確定した駅p(eki_t型)と未確定の駅リスト(eki_t list型)を受け取り *)
(* リストの要素に対して必要な更新を行いそれを返す *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v = let f q = koushin1 p q in List.map f v

(* 以下テスト用 *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 

let lst = [eki1; eki2; eki3; eki4] 

let test1 = koushin1 eki3 eki1 = eki1 
let test2 = koushin1 eki3 eki2 = eki2 
let test3 = koushin1 eki3 eki3 = eki3 
let test4 = koushin1 eki3 eki4 = 
	{namae="後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]} 
let test5 = koushin1 eki2 eki1 = 
	{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]} 
let test6 = koushin1 eki2 eki2 = eki2 
let test7 = koushin1 eki2 eki3 = eki3 
let test8 = koushin1 eki2 eki4 = eki4 

let test9 = koushin eki2 [] = [] 
let test10 = koushin eki2 lst = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 