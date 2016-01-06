(* Debug and reference data structures*)

let adict = [|"boo"; "urns"; "format"; "brillig"; "momewrath"; "jabberwocky"|]

(* Exercise code *)

let rec sort_helper arr counter =
  if (Array.length arr) - 1 = counter then
    true
  else
  if arr.(counter) < arr.(counter + 1) then
    sort_helper arr (counter + 1)
  else
    false;;

let is_sorted a =
  if Array.length a <= 1 then
    true
  else
    sort_helper a 0;;

let rec find dict word =
  if Array.length dict = 0 then
    -1
  else
    let length = Array.length dict in
    let pivot = length / 2 in
    let current_word = dict.(pivot) in

    if current_word = word then
      pivot
    else if length <= 1 then
      -1
    else if word >= current_word then
      find (Array.sub dict pivot (length - pivot)) word
    else if word <= current_word then
      find (Array.sub dict 0 pivot) word
    else
      -1;;
