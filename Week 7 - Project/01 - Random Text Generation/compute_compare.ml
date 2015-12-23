(* Original *)
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

(* Refactored *)
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
