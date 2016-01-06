type queue = int list * int list;;

let is_empty (front, back) =
  match (front, back) with
  | ([], []) -> true
  | (_, _) -> false;;

let enqueue x (front, back) =
  (front, x :: back);;

let rec listbuilder l (front, back) counter =
  if List.length l = counter then
    (front, List.rev back)
  else if counter < List.length l / 2 then
    listbuilder l (front, List.nth l counter :: back) (counter + 1)
  else
    listbuilder l (List.nth l counter :: front, back) (counter + 1);;

let split l =
  listbuilder l ([], []) 0;;

let dequeue (front, back) =
  match (front, back) with
  | ([], back) -> (List.hd (List.rev back), ((List.tl (List.rev back)), []))
  | (front, []) -> (List.hd front, split (List.rev (List.tl front)))
  | (front, back) -> (List.hd front, split (back @ (List.rev (List.tl front))));;
