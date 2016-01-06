type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt;;

exception Unbalanced of int;;

(* Original Functions *)
let rec height = function
  | Empty -> 0
  | Node (t, _, t') -> 1 + (max (height t) (height t'));;

let rec balanced = function
  | Empty -> true
  | Node (t, _, t') ->
    (balanced t) && (balanced t') && height t = height t';;

(* My code *)
let add_tuple t1 t2 =
  let (x1, x2) = t1 and (y1, y2) = t2 in
  (x1 + y1, x2 + y2);;

let rec height = function
  | Empty -> (0, 1)
  | Node (t, _, Empty) -> add_tuple (1, 1) (height t)
  | Node (Empty, _, t') -> add_tuple (1, 1) (height t')
  | Node (t, _, t') ->
    let ht = height t and ht' = height t' in
    let (t1,t2) = ht and (t1', t2') = ht' in
    if t1 > t1' then
      add_tuple (1, t2') (height t)
    else
      add_tuple (1, t2) (height t');;

let rec balanced = function
  | Empty -> (true, 1)
  | Node (t, _, t') ->
    let bal_t = balanced t and bal_t' = balanced t' in
    match (bal_t, bal_t') with
    | ((bool1, count1), (bool2, count2)) ->
      if bool1 && bool2 && height t = height t' then
        (true, count1 + count2 + 2)
      else
        (false, count1 + count2 + 1);;

(* (Node (Node (Node (Empty, -4, Empty), -3, Node (Empty, 2, Empty)), -2, Node (Node (Empty, -4, Empty), -1, Node (Empty, 2, Empty)))) *)


