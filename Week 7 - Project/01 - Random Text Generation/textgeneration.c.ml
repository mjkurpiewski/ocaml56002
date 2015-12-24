open Buffer;;
open Hashtbl;;

type frequencies = (string * int) list;;

type distribution =
  { total : int;
    amounts : frequencies };;

type ptable =
  { prefix_length : int;
    table : (string, distribution) Hashtbl.t }

let character_set = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\128' .. '\255' -> `Char
  | ';' | ',' | ':' | '-' | '"' | '\'' -> `Word_char
  | '?' | '!' | '.' -> `Term
  | _ -> `Separator

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
  let freq_builder (acc : frequencies) (word : string) : frequencies =
    try
      let word_freq = List.assoc word acc in
      (word, word_freq + 1) :: (List.remove_assoc word acc)
    with
    | _ -> (word, 1) :: acc
  in
  { total = List.length words;
    amounts = List.fold_left freq_builder [] words };;
