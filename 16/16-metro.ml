#use "metro.ml"

(* ダイクストラ用の駅情報を格納するレコード型 *)
type eki_t = {namae: string; saitan_kyori: float; temae_list: string list}

(* 駅名の情報を表示する関数 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei = match ekimei with 
    { kanji = a ; kana = b ; romaji = c ; shozoku = d } ->
        d ^ "、 " ^ a ^ "(" ^ b ^ ")"

(* ローマ字の駅名と駅名リストを受け取りその漢字表記を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with 
    [] -> ""
    | {kanji = a; kana = b; romaji = c; shozoku = d} :: rest ->
        if romaji = c then a else romaji_to_kanji romaji rest

(* 漢字の駅名を2つと駅間リストを受け取り2駅間の距離を返す *)
(* 直接つながっていない場合はinfinityを返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori kanji1 kanji2 lst = match lst with
    [] -> infinity
    | {kiten = a; shuten = b; keiyu = c; kyori = d; jikan = e} :: rest ->
        if kanji1 = a && kanji2 = b then d
        else if kanji1 = b && kanji2 = a then d
        else get_ekikan_kyori kanji1 kanji2 rest

(* ローマ字の駅名を2つ受け取り、直接つながっている場合は、繋がっていない場合は、を返す *)
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

(* ekimei_t型のリストを受け取りeki_t型のリストを返す *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let make_eki_list lst = List.map (fun ekimei -> {namae = ekimei.kanji; saitan_kyori = infinity; temae_list = []}) lst

(* eki_t型のリストと起点となる駅名(漢字)の文字列を受け取り、起点のみ距離0、temaeが始点の駅名のみからなるリストを返す *)
(* shokika : eki_t list -> string -> eki_t list *)
let shokika lst shiten = 
    List.map(fun eki ->
    if shiten = eki.namae then {namae = eki.namae; saitan_kyori = 0.0; temae_list = [eki.namae]}
    else eki)
    lst

(* make_eki_listとshokikaを1つにまとめて処理 *)
(* make_initial_eki_list : ekimei_t list -> string ->eki_t list *)
let make_initial_eki_list lst shiten = 
    List.map(fun ekimei ->
    if shiten = ekimei.kanji then {namae = ekimei.kanji; saitan_kyori = 0.0; temae_list = [ekimei.kanji]}
    else {namae = ekimei.kanji; saitan_kyori = infinity; temae_list = []})
    lst

(* 昇順に並んでいる lst の正しい位置に ekimei を挿入する *) 
(* ekimei_insert : ekimei_t list -> ekimei_t -> ekimei_t list *) 
let rec ekimei_insert lst ekimei = match lst with
    [] -> [ekimei]
    | {kanji = a; kana = b; romaji = c; shozoku = d} as first :: rest  -> 
        match ekimei with {kanji = p; kana = q; romaji = r; shozoku = s} -> 
        if b = q then lst 
        else if b < q then first :: ekimei_insert rest ekimei
        else ekimei :: lst

(* ekimei_t型のリストを受け取りひらがな順に整列し駅の重複を取り除いたekimei_t型のリストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec seiretsu lst = match lst with 
    [] -> []
    | first :: rest -> ekimei_insert (seiretsu rest) first 

(* 直前に確定した駅p(eki_t型)と未確定の駅リスト(eki_t list型)を受け取り *)
(* リストの要素に対して必要な更新を行いそれを返す *)
(* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *) 
let koushin p v ekikan_list= 
    List.map(fun q ->
        let kyori = get_ekikan_kyori p.namae q.namae ekikan_list in
            if p.saitan_kyori +. kyori < q.saitan_kyori 
            then {namae = q.namae; saitan_kyori = p.saitan_kyori +. kyori; temae_list = q.namae :: p.temae_list}
            else q) v

(* 駅のリストを受け取り最短距離最小の駅を返す(saitan_wo_bunri用) *)
(* saitan_wo_bunri1 : eki_t list -> eki_t *)
let rec saitan_wo_bunri1 lst = match lst with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
    | first :: rest -> 
        let saitan_eki = saitan_wo_bunri1(rest) in 
        if first.saitan_kyori < saitan_eki.saitan_kyori then first
        else saitan_eki

(* 1つの駅と駅のリストを受け取り後者から前者を除いたものを返す *)
(* saitan_wo_bunri2 : eki_t -> eki_t list -> eki_t list *)
let rec saitan_wo_bunri2 eki lst = List.filter(fun item -> not (item.namae = eki.namae)) lst

(* 駅のリストを受け取り最短距離最小の駅と最短距離最小の駅以外からなるリストの組を返す *)
(* TODO： fold_rightでリファクタ *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri lst = 
    let saitan_eki = saitan_wo_bunri1 lst in
    (saitan_eki, saitan_wo_bunri2 saitan_eki lst)

(* 駅リスト(eki_t list)と駅間のリスト(ekikan_t list)を受け取り *)
(* ダイクストラで最短距離と最短経路が入ったリスト(eki_t list)を返す *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_list ekikan_list = match eki_list with
    [] -> []
    | first :: rest ->
    let (saitan, nokori) = saitan_wo_bunri eki_list in
    let new_eki_list = koushin saitan nokori ekikan_list in 
    saitan :: dijkstra_main new_eki_list ekikan_list

(* 駅名(漢字)と駅リストを受け取り、該当のレコード(eki_t)を返す *)
let rec find kanji_ekimei eki_list = match eki_list with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
    | first :: rest ->
    if first.namae = kanji_ekimei then first
    else find kanji_ekimei rest

(* 始点の駅名(ローマ字)と終点の駅名(ローマ字)を受け取り *)
(* 最短距離を計算して終点のレコード(eki_t)を返す *)
(* djkstra : string -> string -> eki_t *)
let dijkstra romaji_shiten romaji_shuten =
    let ekimei_list = seiretsu global_ekimei_list in
    let kanji_shiten = romaji_to_kanji romaji_shiten ekimei_list in 
    let kanji_shuten = romaji_to_kanji romaji_shuten ekimei_list in
    let eki_list = make_initial_eki_list ekimei_list kanji_shiten in
    let new_eki_list = dijkstra_main eki_list global_ekikan_list in 
    find kanji_shuten new_eki_list

(* romaji_to_kanji テスト *)
let test1 = romaji_to_kanji "hoge" global_ekimei_list = ""
let test2 = romaji_to_kanji "yoyogiuehara" global_ekimei_list = "代々木上原"
let test3 = romaji_to_kanji "yushima" global_ekimei_list = "湯島"

(* get_ekikan_kyori テスト *)
let test4 = get_ekikan_kyori "東京" "表参道" global_ekikan_list = infinity
let test5 = get_ekikan_kyori "渋谷" "表参道" global_ekikan_list = 1.3
let test6 = get_ekikan_kyori "表参道" "外苑前" global_ekikan_list = 0.7

(* kyori_wo_hyoji テスト *)
let test7 = kyori_wo_hyoji "hoge" "omotesandou" global_ekimei_list global_ekikan_list
  = "hogeという駅は存在しません"
let test8 = kyori_wo_hyoji "tokyo" "omotesandou" global_ekimei_list global_ekikan_list
  = "東京駅と表参道駅はつながっていません"
let test9 = kyori_wo_hyoji "shibuya" "omotesandou" global_ekimei_list global_ekikan_list
  = "渋谷駅から表参道駅までは1.3kmです" 
let test10 = kyori_wo_hyoji "omotesandou" "gaienmae" global_ekimei_list global_ekikan_list
  = "表参道駅から外苑前駅までは0.7kmです"

(* make_eki_list テスト *)
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

(* shokika テスト *)
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

(* seiretsu テスト *)
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

(* koushin テスト *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 

let lst = [eki1; eki2; eki3; eki4] 

let test1 = koushin eki2 [] global_ekikan_list = [] 
let test2 = koushin eki2 lst global_ekikan_list = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 

(* make_initial_eki_list テスト *)
let ekimei1 = {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"}
let ekimei2 = {kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; shozoku="千代田線"}
let ekimei3 = {kanji="赤坂"; kana="あかさか"; romaji="akasaka"; shozoku="千代田線"}

let ekimei_list1 = [ekimei1; ekimei2]
let ekimei_list2 = [ekimei1; ekimei2; ekimei3]

let test1 = make_initial_eki_list ekimei_list1 "表参道" = [
  {namae = "表参道"; saitan_kyori = 0.; temae_list = ["表参道"]};
  {namae = "乃木坂"; saitan_kyori = infinity; temae_list = []}
]
let test2 = make_initial_eki_list ekimei_list2 "赤坂" = [
  {namae = "表参道"; saitan_kyori = infinity; temae_list = []};
  {namae = "乃木坂"; saitan_kyori = infinity; temae_list = []};
  {namae = "赤坂"; saitan_kyori = 0.; temae_list = ["赤坂"]}
]

(* saitan_wo_bunriテスト *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
let lst = [eki1; eki2; eki3; eki4] 
 
let test = saitan_wo_bunri lst = (eki3, [eki1; eki2; eki4]) 

(* djkstra_mainテスト *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = infinity; temae_list = []} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 

let lst = [eki1; eki2; eki3; eki4] 

let test1 = dijkstra_main [] global_ekikan_list = [] 
let test2 = dijkstra_main lst global_ekikan_list = 
  [{namae = "茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}; 
   {namae = "新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]}; 
   {namae = "後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]}; 
   {namae = "池袋"; saitan_kyori = 3.; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}] 

(* dijkstraテスト *)
let test1 = dijkstra "shibuya" "gokokuji" = 
  {namae = "護国寺"; saitan_kyori = 9.8; 
   temae_list = 
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; 
      "青山一丁目"; "表参道"; "渋谷"]} 
let test2 = dijkstra "myogadani" "meguro" = 
  {namae = "目黒"; saitan_kyori = 12.7000000000000028; 
   temae_list = 
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王"; 
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]} 