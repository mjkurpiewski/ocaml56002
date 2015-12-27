open Buffer;;
open Hashtbl;;

exception Prefix_mismatch;;

type distribution =
  { total : int;
    amounts : (string * int) list};;

type ptable =
  { prefix_length : int;
    table : (string list, distribution) Hashtbl.t};;

let merge_distributions
    (distribution1 : distribution)
    (distribution2 : distribution) =

  let rec frequency_merger
      (shorter : (string * int) list)
      (longer : (string * int) list) : (string * int) list =
    match shorter with
    | [] -> longer
    | (name, freq) :: tl ->
      if List.mem_assoc name longer then
        let longer_freq = List.assoc name longer in
        frequency_merger tl ((name, freq + longer_freq) :: (List.remove_assoc name longer))
      else
        frequency_merger tl ((name, freq) :: longer)
  in

  {total = distribution1.total + distribution2.total;
   amounts = frequency_merger distribution1.amounts distribution2.amounts};;

let character_set = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\128' .. '\255' -> `Char
  | ';' | ',' | ':' | '-' | '"' | '\'' -> `Word_char
  | '?' | '!' | '.' -> `Term
  | _ -> `Separator

let print_table (input : ptable) =
  let table = input.table in
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) table [];;

let words (str : string) : string list =
  let w_buffer = Buffer.create 16 in
  let input_length = String.length str in

  let char_to_string (c : char) : string =
    String.make 1 c
  in

  let rec wordlist_builder (acc : string list) (idx : int) =
    if idx = input_length && Buffer.contents w_buffer <> "" then
      (Buffer.contents w_buffer) :: acc
    else if idx = input_length then
      acc
    else
      let this_char = str.[idx] in
      match (character_set this_char) with
      | `Char -> (Buffer.add_char w_buffer this_char; wordlist_builder acc (idx + 1))
      | `Term | `Word_char ->
        (let word = Buffer.contents w_buffer in
         Buffer.reset w_buffer;
         wordlist_builder (char_to_string this_char :: word :: acc) (idx + 1))
      | `Separator ->
        (let word = Buffer.contents w_buffer in
         Buffer.reset w_buffer;
         if word <> "" then
           wordlist_builder (word :: acc) (idx + 1)
         else
           wordlist_builder acc (idx + 1))
  in
  List.rev (wordlist_builder [] 0);;

let sentences (str : string) : string list list =
  let to_split = List.filter (fun x -> x <> "") (words str) in

  let rec sentences_builder
      (acc : string list list)
      (current_sentence : string list)
      (input_words : string list) =
    match input_words with
    | [] ->
      if acc = [] then
        ((List.rev current_sentence) :: acc)
      else
        acc
    | hd :: [] -> sentences_builder (List.rev (hd :: current_sentence) :: acc) [] []
    | hd :: tl ->
      match hd with
      | "!" | "." | "?" ->
        let complete_sentence = List.rev (hd :: current_sentence) in
        sentences_builder (complete_sentence :: acc) [] tl
      | _ ->
        sentences_builder acc (hd :: current_sentence) tl
  in
  List.rev (sentences_builder [] [] to_split);;

let compute_distribution (words : string list) : distribution =
  let freq_builder acc (word : string) =
    try
      let word_freq = List.assoc word acc in
      (word, word_freq + 1) :: (List.remove_assoc word acc)
    with
    | _ -> (word, 1) :: acc
  in
  { total = List.length words;
    amounts = List.fold_left freq_builder [] words };;

let start (prefix_size : int) : string list =
  let rec prefix_builder (prefix : string list) : string list =
    if List.length prefix = prefix_size then
      prefix
    else
      prefix_builder ("START" :: prefix)
  in
  prefix_builder [];;

let shift (elements : string list) (new_element : string) : string list =
  match elements with
  | [] -> []
  | hd :: [] -> [new_element]
  | hd :: tl -> List.rev (new_element :: List.rev tl);;

let build_ptable (words : string list) (prefix_size : int) =
  let start_prefix = start prefix_size in
  let interim_hash = Hashtbl.create (List.length words) in
  let final_hash = Hashtbl.create (List.length words) in

  let rec interim_builder
      (words : string list)
      (working_prefix : string list)
      interim_table =

    match words with
    | [] ->
      if Hashtbl.mem interim_table working_prefix then
        let values = Hashtbl.find interim_table working_prefix in
        (Hashtbl.replace interim_table working_prefix ("STOP" :: values);
         interim_table)
      else
        (Hashtbl.add interim_table working_prefix ["STOP"];
         interim_table)

    | hd :: [] ->
      let local_builder = (fun _ -> interim_builder
                              []
                              (shift working_prefix hd)
                              interim_table) in

      if Hashtbl.mem interim_table working_prefix then
        let values = Hashtbl.find interim_table working_prefix in
        (Hashtbl.replace interim_table working_prefix (hd :: values);
         local_builder ())
      else
        (Hashtbl.add interim_table working_prefix [hd];
         local_builder ())

    | hd :: tl ->
      let local_builder = (fun _ -> interim_builder
                              tl
                              (shift working_prefix hd)
                              interim_table) in

      if Hashtbl.mem interim_table working_prefix then
        let values = Hashtbl.find interim_table working_prefix in
        (Hashtbl.replace interim_table working_prefix (hd :: values);
         local_builder ())
      else
        (Hashtbl.add interim_table working_prefix [hd];
        local_builder ())

  in

  Hashtbl.add interim_hash start_prefix [];

  let constructed_hash = interim_builder words start_prefix interim_hash in

  Hashtbl.iter
    (fun x y ->
       Hashtbl.add final_hash x (compute_distribution y))
    constructed_hash;

  {prefix_length = prefix_size; table = final_hash};;

let next_in_ptable corpus (prefix : string list) : string =
  let random_float = Random.float 1.0 in

  let word_distribtuion = Hashtbl.find corpus prefix in
  let total_weight = word_distribtuion.total in
  let candidates = word_distribtuion.amounts in

  let rec aux candidates (placeholder : float) : string =
    match candidates with
    | [] -> ""
    | (word, occur) :: [] -> word
    | (word, occur) :: tl ->
      let local_probability =
        float_of_int occur /. float_of_int total_weight in

      let relative_probability = local_probability +. placeholder in

      if (random_float >= placeholder) &&
         (random_float < relative_probability) then
        word
      else
        aux tl relative_probability
  in
  aux candidates 0.0;;

let walk_ptable { table = corpus ; prefix_length = prefix_size } : string list =
  let rec walk_helper
      (sentence : string list)
      (current_prefix : string list) =
    let next_word = next_in_ptable corpus current_prefix in
    if next_word = "STOP" then
      List.rev sentence
    else
      walk_helper (next_word :: sentence) (shift current_prefix next_word)
  in
  walk_helper [] (start prefix_size);;

let merge_ptables (ptables : ptable list) : ptable =
  let merged_table = List.hd ptables in

  let rec table_walker (merging : ptable) (to_merge : ptable list) : ptable =
    match to_merge with
    | [] -> merging
    | hd :: tl ->
      let next_table = hd in
      if next_table.prefix_length <> merging.prefix_length then
        raise Prefix_mismatch
      else
        Hashtbl.iter
          (fun x y ->
             if Hashtbl.mem merging.table x then
               let old_y = Hashtbl.find merging.table x in
               Hashtbl.replace merging.table x (merge_distributions y old_y)
             else
               Hashtbl.add merging.table x y)
        next_table.table;
      table_walker merging tl
  in
  table_walker merged_table (List.tl ptables);;
