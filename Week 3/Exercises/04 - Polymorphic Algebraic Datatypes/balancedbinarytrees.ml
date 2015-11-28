type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

let height (tree : 'a bt) : int =
  let rec height_helper (tree : 'a bt) (counter : int) : int =
    match tree with
    | Empty -> counter
    | Node (Empty, _, r) -> (height_helper r (counter + 1))
    | Node (l, _, Empty) -> (height_helper l (counter + 1))
    | Node (l, _, r) -> (height_helper l (counter + 1)) +
                        (height_helper r (counter + 1))
  in
  height_helper tree 0;;

let rec height t =
  match t with
  | Empty -> 0
  | Node (Empty, _, r) -> 1 + (height r)
  | Node (l, _, Empty) -> 1 + (height l)
  | Node (l, _, r) -> 1 + (height r) + (height l);;
