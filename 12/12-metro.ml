(* 駅名の情報を格納するレコード型 *)
type ekimei_t = { kanji : string ; kana : string ; romaji : string ; shozoku : string }

(* 駅間の情報を格納するレコード型 *)
type ekikan_t = { kiten : string ; shuten : string ; keiyu : string ; kyori : float ; jikan : int}

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

(* 以下テスト用 メンドウなのでコピペした *)
let ekimei1 = {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"}
let ekimei2 = {kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; shozoku="千代田線"}
let ekimei3 = {kanji="赤坂"; kana="あかさか"; romaji="akasaka"; shozoku="千代田線"}

let ekimei_list1 = [ekimei1]
let ekimei_list2 = [ekimei1; ekimei2]
let ekimei_list3 = [ekimei1; ekimei2; ekimei3]

let test1 = make_eki_list [] = []
let test2 = make_eki_list ekimei_list1 = [
  {namae = ekimei1.kanji; saitan_kyori = infinity; temae_list = []}
]
let test3 = make_eki_list ekimei_list2 = [
  {namae = ekimei1.kanji; saitan_kyori = infinity; temae_list = []};
  {namae = ekimei2.kanji; saitan_kyori = infinity; temae_list = []}
]
let test4 = make_eki_list ekimei_list3 = [
  {namae = ekimei1.kanji; saitan_kyori = infinity; temae_list = []};
  {namae = ekimei2.kanji; saitan_kyori = infinity; temae_list = []};
  {namae = ekimei3.kanji; saitan_kyori = infinity; temae_list = []}
]

let eki_list = [
  {namae = "表参道"; saitan_kyori = infinity; temae_list = []};
  {namae = "乃木坂"; saitan_kyori = infinity; temae_list = []};
  {namae = "赤坂"; saitan_kyori = infinity; temae_list = []}
]

let test5 = shokika eki_list "表参道" = [
  {namae = "表参道"; saitan_kyori = 0.; temae_list = ["表参道"]};
  {namae = "乃木坂"; saitan_kyori = infinity; temae_list = []};
  {namae = "赤坂"; saitan_kyori = infinity; temae_list = []}
]
let test6 = shokika eki_list "赤坂" = [
  {namae = "表参道"; saitan_kyori = infinity; temae_list = []};
  {namae = "乃木坂"; saitan_kyori = infinity; temae_list = []};
  {namae = "赤坂"; saitan_kyori = 0.; temae_list = ["赤坂"]}
]

let ekimei_list = [ 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
{kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"}; 
{kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"}; 
{kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"}; 
{kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"}; 
{kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"} 
] 

let test7 = seiretsu [] = [] 
let test8 = seiretsu ekimei_list = [ 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
{kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"}; 
{kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"}; 
{kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"}; 
{kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"}; 
{kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"} 
] 