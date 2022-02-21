#use "metro.ml"

(* ダイクストラ用の駅情報を格納するレコード型 *)
type eki_t = {namae: string; saitan_kyori: float; temae_list: string list}

type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

exception Not_found

exception No_such_station of string

(* ローマ字の駅名と駅名リストを受け取りその漢字表記を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with 
    [] -> raise (No_such_station(romaji))
    | {kanji = a; kana = b; romaji = c; shozoku = d} :: rest ->
        if romaji = c then a else romaji_to_kanji romaji rest

(* 駅名と駅名と距離の組のリストを受け取りその距離を返す *)
(* 駅名が存在しない場合はinfinityを返す *)
(* assoc: string -> (string * float) list -> float *)
let rec assoc ekimei lst = match lst with
    [] -> raise Not_found
    | (eki, kyori) :: rest -> 
        if ekimei = eki then kyori
        else assoc ekimei rest

(* ekikan_tree_t型の木とekimei1,ekimei2,kyoriを受け取り *)
(* ekimei1のノードに{ekimei2,kyori}を挿入した木を返す *)
(* insert_half: ekikan_tree_t -> string -> string ->float ->ekikan_tree_t *)
let rec insert_half tree ekimei1 ekimei2 kyori = match tree with
    | Empty -> Node(Empty, ekimei1,[(ekimei2, kyori)], Empty)
    | Node(t1, ekimei, lst, t2) ->
        if ekimei1 = ekimei then Node(t1, ekimei, (ekimei2, kyori)::lst, t2)
        else if ekimei1 < ekimei then Node(insert_half t1 ekimei1  ekimei2 kyori, ekimei, lst, t2)
        else Node(t1, ekimei, lst, insert_half t2 ekimei1 ekimei2 kyori)

(* ekikan_tree_t型の木とekitan_t型の駅間を受け取り *)
(* その情報を挿入した木を返す *)
(* insert_ekikan: ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let rec insert_ekikan tree {kiten = a; shuten = b; keiyu = c; kyori = d; jikan = e} =
    let tmp = insert_half tree a b d in insert_half tmp b a d

(* ekikan_tree_t型の木とekikan_t list型の駅間リストを受け取り *)
(* リストに含まれる駅間を全て挿入した木を返す *)
(* inserts_ekikan: ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan tree lst = List.fold_left insert_ekikan tree lst

(* 二つの駅が繋がっていない場合例外を起こすようにリファクタ *)
(* 二分探索木でリファクタ *)
(* 漢字の駅名を2つと駅間リストを受け取り2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t_tree-> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 tree = match tree with
    | Empty -> raise Not_found
    | Node(t1, ekimei, lst, t2) ->
        if ekimei = ekimei1 then assoc ekimei2 lst
        else if ekimei1 < ekimei then get_ekikan_kyori ekimei1 ekimei2 t1
        else get_ekikan_kyori ekimei1 ekimei2 t2

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

(* get_ekikan_kyoriを例外を返す形にリファクタしたのでそれに伴う修正 *)
(* ekikan_tree_tを受け取る形にリファクタ *)
(* 直前に確定した駅p(eki_t型)と未確定の駅リスト(eki_t list型)とekikan_tree_tを受け取り *)
(* リストの要素に対して必要な更新を行いそれを返す *)
(* koushin : eki_t -> eki_t list -> ekikan_tree_t -> eki_t list *) 
let koushin p v tree =
    List.map(fun q -> try
        let kyori = get_ekikan_kyori p.namae q.namae tree in
            if p.saitan_kyori +. kyori < q.saitan_kyori 
            then {namae = q.namae; saitan_kyori = p.saitan_kyori +. kyori; temae_list = q.namae :: p.temae_list}
            else q
            with Not_found -> q) v

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
(* TODO： 17.16の形式に従い書き換え *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri lst = 
    let saitan_eki = saitan_wo_bunri1 lst in
    (saitan_eki, saitan_wo_bunri2 saitan_eki lst)

(* 駅リスト(eki_t list)と駅間のリスト(ekikan_t list)を受け取り *)
(* ダイクストラで最短距離と最短経路が入ったリスト(eki_t list)を返す *)
(* dijkstra_main : eki_t list -> ekikan_tree_t -> eki_t list *)
let rec dijkstra_main eki_list tree = match eki_list with
    [] -> []
    | first :: rest ->
    let (saitan, nokori) = saitan_wo_bunri eki_list in
    let new_eki_list = koushin saitan nokori tree in 
    saitan :: dijkstra_main new_eki_list tree

(* 駅名(漢字)と駅リストを受け取り、該当のレコード(eki_t)を返す *)
let rec find kanji_ekimei eki_list = match eki_list with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
    | first :: rest ->
    if first.namae = kanji_ekimei then first
    else find kanji_ekimei rest

(* 始点の駅名(ローマ字)と終点の駅名(ローマ字)を受け取り *)
(* 最短距離を計算して終点のレコード(eki_t)を返す *)
(* グローバル駅間リストは引数として渡していない *)
(* djkstra : string -> string -> eki_t *)
let dijkstra romaji_shiten romaji_shuten =
    let ekimei_list = seiretsu global_ekimei_list in
    let kanji_shiten = romaji_to_kanji romaji_shiten ekimei_list in 
    let kanji_shuten = romaji_to_kanji romaji_shuten ekimei_list in
    let eki_list = make_initial_eki_list ekimei_list kanji_shiten in
    let ekikan_tree = inserts_ekikan Empty global_ekikan_list in
    let new_eki_list = dijkstra_main eki_list ekikan_tree in 
    find kanji_shuten new_eki_list

(* romaji_to_kanji テスト *)
(* test1は例外発生→動作確認だけしてコメントアウト *)
(* let test1 = romaji_to_kanji "hoge" global_ekimei_list = "" *)
let test2 = romaji_to_kanji "yoyogiuehara" global_ekimei_list = "代々木上原"
let test3 = romaji_to_kanji "yushima" global_ekimei_list = "湯島"

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

(* assocのテスト *)
(* 例外はキャッチしない場合そのまま処理が終了してしまうのでコメントアウト *)
(* let test1 = assoc "後楽園" [] = infinity 
let test2 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8 
let test3 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity  *)

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

(* insert_ekikanのテスト *)
(* メンドウなので一旦省略  *)

(* inserts_ekikanのテスト *)
(* メンドウなので一旦省略  *)