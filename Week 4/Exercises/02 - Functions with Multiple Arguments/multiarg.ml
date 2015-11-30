(* Given *)
let rec equal_on_common l1 l2 =
  match l1,l2 with
  | [],_ -> true
  | _,[] -> true
  | h1::r1, h2::r2 -> h1 = h2 && equal_on_common r1 r2;;

(* First draft, can't use ||, List.tl, List.hd *)
let rec equal_on_common =
  function x ->
  function y ->
    if x = [] && y = [] then
      true
    else
      List.hd x = List.hd y && equal_on_common (List.tl x) (List.tl y);;

(* 25/25!!! *)
let rec equal_on_common =
  function
  | [] -> fun x -> true
  | hd1 :: tl1 -> function
    | [] -> true
    | hd2 :: tl2 ->
      hd1 = hd2 && equal_on_common tl1 tl2;;
