open Buffer;;
type ltable = (string * string list) list;;
type freq_list = (string * int) list;;

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

let freq_generator (words : string list) : freq_list =
  let freq_builder (acc : freq_list) (word : string) : freq_list =
    if List.mem_assoc word acc then
      let word_freq = List.assoc word acc in
      (word, word_freq + 1) :: (List.remove_assoc word acc)
    else
      (word, 1) :: acc
  in
  let sorted = List.sort String.compare words in
  List.fold_left freq_builder [] sorted;;

let return_highest_freq (frequencies : freq_list) : string =
    let rec aux (frequencies : freq_list) (to_return : string * int) =
      match frequencies with
      | [] -> fst to_return
      | (name, frq) :: tl ->
        if frq > snd to_return then
          aux tl (name, frq)
        else
          aux tl to_return
    in
    aux frequencies ("", 0);;

let next_in_ltable (table : ltable) (word : string) : string =
  if not (List.mem_assoc word table) then
    raise Not_found
  else
    let candidates = List.assoc word table in
    List.nth candidates (Random.int (List.length candidates));;

let walk_ltable (corpus : ltable) : string list =
  let rec walk_helper (sentence : string list) (last_word : string) =
    let next_word = next_in_ltable corpus last_word in
    if next_word = "STOP" then
      List.rev sentence
    else
      walk_helper (next_word :: sentence) next_word
  in
  walk_helper [] "START";;
