type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

let rec height t =
  match t with
  | Empty -> 0
  | Node (Empty, _, r) -> 1 + (height r)
  | Node (l, _, Empty) -> 1 + (height l)
  | Node (l, _, r) -> 1 + max (height r) (height l);;

let rec balanced t =
  match t with
  | Empty -> true
  | Node (Empty, _, Empty) -> true
  | Node (l, _, r) ->
    if (height l) = (height r) then
      balanced l && balanced r
    else
      false;;
