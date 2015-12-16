let mandog = words "I am a man and my dog is a good dog and a good dog makes a good man";;

(fun h -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []) dogtbl;;

let build_ltable (words : string list) : ltable =
  let init_table = [("START"), [List.hd words]] in

  let rec ltable_builder (words : string list) (table : ltable) : ltable =
    match words with
    | [] -> table

    | hd :: [] ->
      let local_builder = (fun x -> ltable_builder [] x) in
      if List.mem_assoc hd table then
        let keys = List.assoc hd table in
        local_builder ((hd, "STOP" :: keys) :: (List.remove_assoc hd table))
      else
        local_builder ((hd, ["STOP"]) :: table)

    | hd :: nx :: tl ->
      let local_builder = (fun x -> ltable_builder (nx :: tl) x) in
      if List.mem_assoc hd table then
        let keys = List.assoc hd table in
        local_builder ((hd, nx :: keys) :: (List.remove_assoc hd table))
      else
        local_builder ((hd, [nx]) :: table)

  in ltable_builder words init_table;;
