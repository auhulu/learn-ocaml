type distance_t = {
  kyori: float; (* 隣り合う点との距離 *)
  total: float; (* 先頭からの距離の合計 *)
}

(* 先頭からリスト中の各点までの距離の合計を計算して返す *)
(* hojoを使わずとも一応実装可能‘ *)
(* total_distance : distance_t list -> distance_t list *)
let rec total_distance lst = match lst with 
    [] -> []
    | first :: rest -> {kyori = first.kyori; total = first.kyori} :: List.map(fun dist ->{kyori = dist.kyori; total = dist.total +. first.kyori}) (total_distance rest)

(* 先頭からリスト中の各点までの距離の合計を計算して返す*)
(* 引数調整用の補助関数 *)
(* hojo : distance_t list -> float -> distance_t list *)
let rec hojo lst total = match lst with
    [] -> []
    | first :: rest -> {kyori = first.kyori; total = first.kyori +. total} :: hojo rest (first.kyori +. total)

let total_distance lst = hojo lst 0.0

let lst = [
  {kyori = 0.3; total = 0.};
  {kyori = 0.9; total = 0.};
  {kyori = 1.4; total = 0.};
  {kyori = 0.8; total = 0.}
]

let test = total_distance lst