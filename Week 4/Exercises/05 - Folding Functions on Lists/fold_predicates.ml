let for_all p l =
  List.fold_left
    (fun boolean element ->
       if p element && boolean then
         true
       else
         false)
    true
    l;;

let exists p l =
  List.fold_left
    (fun boolean element ->
       if p element then
         true
       else if boolean then
         true
       else
         false) false l;;

let sorted cmp (l : 'a list) =
  if l = [] then
    true
  else if
    (List.fold_left
       (fun (acc : 'a option) (element : 'a) ->
          match acc with
          | None -> None
          | Some (v) ->
            if cmp v element <= 0 then
              Some (element)
            else
              None)
       (Some (List.hd l))
       (List.tl l))
    = None
  then
    false
  else
    true;;
