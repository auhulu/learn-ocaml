(* 級数のn項を求める *)
(* n_kou : int -> float *)
let rec n_kou n = if n = 0 then 1.0 else n_kou (n - 1) /. float_of_int n

(* ネイピア数の近似解を求める *)
(* この実装は引数を0しか指定できないのでは？ *)
(* e : int -> float *)
let rec error_e n = 
    let tmp = n_kou n in
    if tmp < 0.00001 then tmp else tmp +. error_e (n + 1)


