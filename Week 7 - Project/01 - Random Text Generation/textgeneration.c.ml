open Buffer;;
open Hashtbl;;

type frequencies = (string * int) list;;

type distribution =
  { total : int;
    amounts : frequencies };;

type ptable =
  { prefix_length : int;
    table : (string, distribution) Hashtbl.t }

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
