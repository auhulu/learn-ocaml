type nengou_t = 
    | Meiji of int 
    | Taisho of int
    | Showa of int
    | Heisei of int

(* 年号を受け取り対応する西暦を返す *)
(* to_seireki : nengou_t -> int *)
let to_seireki nengou = match nengou with
    | Meiji (n) -> n + 1867
    | Taisho (n) -> n + 1911
    | Showa (n) -> n + 1925
    | Heisei (n) -> n + 1988

(* 誕生年と現在の年を受け取り年齢を返す *)
(* nenrei : nengou_t -> nengou_t -> int *)
let nenrei birth now = to_seireki(now) - to_seireki(birth)

let test = nenrei (Showa(20)) (Heisei(9)) = 52
