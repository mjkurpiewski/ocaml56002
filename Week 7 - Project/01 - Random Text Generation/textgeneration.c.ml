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
  | ';' | ',' | ':' | '-' | '"' | '\'' -> `Word
  | '?' | '!' | '.' -> `Term
  | _ -> `Separator

let is_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\128' .. '\255' -> true
  | _ -> false;;

let is_terminator = function
  | '?' | '!' | '.' -> true
  | _ -> false;;

let is_special_char = function
  | ';' | ',' | ':' | '-' | '"' | '\'' | '?' | '!' | '.' -> true
  | _ -> false;;

let words (str : string) : string list =
  let w_buffer = Buffer.create 16 in

  let rec aux (i : int) (acc : string list) : string list =
    let to_add = Buffer.contents w_buffer in

    let empty_strp ?f:(f = fun x -> x) (str : string) =
      if str <> "" then
        f (to_add :: acc)
      else
        f acc
    in

    if i = String.length str then
      empty_strp to_add
    else
      let char_at = str.[i] in
      match char_at with
      | c when is_character char_at -> (Buffer.add_char w_buffer c; aux (i + 1) acc)
      | c when is_special_char char_at ->
        (Buffer.clear w_buffer;
        aux (i + 1) ((Char.escaped c) :: to_add :: acc))
      | _ ->
        (Buffer.clear w_buffer;
        empty_strp to_add ~f:(fun x -> aux (i + 1) x))
  in
  List.rev (aux 0 []);;

let sentences (str : string) : string list =
  let sentence_list = [] in

  let rec splitter (sentences : string list) (current_idx : int) (previous_idx : int) =
    if String.length str = current_idx && previous_idx = 0 then
      [str]
    else
      try
        let char_at = str.[current_idx] in
        match char_at with
        | c when is_terminator char_at ->
          splitter
            ((String.sub str previous_idx (current_idx - previous_idx + 1)) :: sentences)
            (current_idx + 1)
            (current_idx + 1)
        | c -> splitter
                 sentences
                 (current_idx + 1)
                 previous_idx
      with
      | _ -> List.rev sentences
  in
  (splitter sentence_list 0 0);;

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
