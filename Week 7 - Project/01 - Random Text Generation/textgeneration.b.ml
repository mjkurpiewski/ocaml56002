open Buffer;;
open Hashtbl;;

type frequencies = (string * int) list;;

type distribution =
  { total : int;
    amounts : frequencies };;

type htable = (string, distribution) Hashtbl.t;;

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

let compute_distribution (words : string list) : distribution =
  let freq_builder (acc : frequencies) (word : string) : frequencies =
    if List.mem_assoc word acc then
      let word_freq = List.assoc word acc in
      (word, word_freq + 1) :: (List.remove_assoc word acc)
    else
      (word, 1) :: acc
  in
  let sorted = List.sort String.compare words in
  { total = List.length words;
    amounts = List.fold_left freq_builder [] sorted};;

let build_htable (words : string list) =
  let interim_hash = Hashtbl.create (List.length words) in
  let final_hash = Hashtbl.create (List.length words) in

  let rec interim_builder words interim_table =
      match words with
    | [] -> interim_table

    | hd :: [] ->
      let local_builder = (fun _ -> interim_builder [] interim_table) in

      if Hashtbl.mem interim_table hd then
        let values = Hashtbl.find interim_table hd in
        (Hashtbl.replace interim_table hd ("STOP" :: values); local_builder ())
      else
        (Hashtbl.add interim_table hd ["STOP"]; local_builder ())

    | hd :: nx :: tl ->
      let local_builder = (fun _ -> interim_builder (nx :: tl) interim_table) in

      if Hashtbl.mem interim_table hd then
        let values = Hashtbl.find interim_table hd in
        (Hashtbl.replace interim_table hd (nx :: values); local_builder ())
      else
        (Hashtbl.add interim_table hd [nx]; local_builder ())
  in

  Hashtbl.add interim_hash "START" [List.hd words];

  let constructed_hash = interim_builder words interim_hash in

  Hashtbl.iter
    (fun x y ->
       Hashtbl.add final_hash x (compute_distribution y))
    constructed_hash;

  final_hash;;

let next_in_htable (corpus : htable) (word : string) : string =
  let word_distribtuion = Hashtbl.find corpus word in
  let total_weight = word_distribtuion.total in
  let candidates = word_distribtuion.amounts in
  let random_float = Random.float 1.0 in

  let rec aux (candidates : frequencies) (placeholder : float) : string =
    match candidates with
    | [] -> ""
    | (word, occur) :: [] -> word
    | (word, occur) :: tl ->
      let local_probability = float_of_int occur /. float_of_int total_weight in
      let relative_probability = local_probability +. placeholder in
      if random_float >= placeholder && random_float < relative_probability then
        word
      else
        aux tl relative_probability
  in
  aux candidates 0.0;;

let walk_htable (corpus : htable) : string list =
  let rec walk_helper (sentence : string list) (last_word : string) =
    let next_word = next_in_htable corpus last_word in
    if next_word = "STOP" then
      List.rev sentence
    else
      walk_helper (next_word :: sentence) next_word
  in
  walk_helper [] "START";;
