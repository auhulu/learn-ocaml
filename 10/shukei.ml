type gakusei_t = { namae : string ; tensuu : int ; seiseki : string }
let gakusei_list = [
  {namae = "鈴木"; tensuu = 80; seiseki = "A"};
  {namae = "山田"; tensuu = 70; seiseki = "B"};
  {namae = "高橋"; tensuu = 85; seiseki = "A"}
]

(* 目的 : 学生リストのうち各成績の人数を集計 *)
(* shukei : gakusei_t list -> int * int * int * int *)
let rec shukei lst = match lst with 
    [] -> (0, 0, 0, 0)
    | {seiseki = s} :: rest -> 
        let (a, b, c, d) = shukei rest in 
        if s = "A" then (a + 1, b, c, d)
        else if s = "B" then (a, b + 1, c, d)
        else if s = "C" then (a, b, c + 1, d)
        else (a, b, c, d + 1)

let test = shukei gakusei_list = (2, 1, 0, 0)
