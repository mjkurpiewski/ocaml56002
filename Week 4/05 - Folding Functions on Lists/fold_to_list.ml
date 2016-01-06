let filter p l =
  List.fold_right
    (fun element ln ->
      if p element then
        element :: ln
      else
        ln)
    l
    [];;

let partition p l =
  let lpos (lpos, _) = lpos and lneg (_, lneg) = lneg in
  List.fold_right
    (fun element tup ->
       if p element then
         ((element :: lpos tup), lneg tup)
       else
         (lpos tup, (element :: lneg tup)))
    l
    ([], []);;

let rec sort =
  function (l : 'a list) ->
  match l with
  | [] -> []
  | h :: [] -> [h]
  | h :: r ->
    let (lh, gh) =
      partition (fun x -> if x <= h then true else false) r in
    List.append
      (List.append (sort lh) [h])
      (sort gh);;
