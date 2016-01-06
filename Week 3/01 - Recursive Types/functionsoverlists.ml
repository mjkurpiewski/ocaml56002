let rec mem x l =
  match l with
  | [] -> false
  | hd :: tl -> if hd = x then
      true
    else
      mem x tl;;

let rec append l1 l2 =
  match (l1, l2) with
  | (l1, []) -> l1
  | (l1, hd :: tl) -> append (l1 @ [hd]) tl;;

let rec combine l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (hd1 :: tl1, hd2 :: tl2) -> (hd1, hd2) :: combine tl1 tl2;;

let rec assoc l k =
  match l with
  | [] -> None
  | hd :: tl -> if (fst hd) = k then
      (Some (snd hd))
    else
      assoc tl k;;
