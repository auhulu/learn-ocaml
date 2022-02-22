(* #use "metro.ml" *)
open Metro_data

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

(* 二分探索木のモジュールを使用した形にリファクタ *)
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

(* let rec get_ekikan_kyori ekimei1 ekimei2 tree = 
    let tmp_lst = Tree.search tree ekimei1
    in  assoc ekimei2 tmp_lst *)

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