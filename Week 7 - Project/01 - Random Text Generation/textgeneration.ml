open Buffer;;
type ltable = (string * string list) list;;

let words (str : string) : string list =
  let w_buffer = Buffer.create 16 in
  let rec aux (i : int) (acc : string list) : string list =
    let to_add = Buffer.contents w_buffer in
    if i = String.length str then
      to_add :: acc
    else
      let char_at = str.[i] in
      match char_at with
      | ' ' -> (Buffer.clear w_buffer; aux (i + 1) (to_add :: acc))
      | c -> (Buffer.add_char w_buffer c; aux (i + 1) acc)
  in
  List.rev (aux 0 []);;

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
