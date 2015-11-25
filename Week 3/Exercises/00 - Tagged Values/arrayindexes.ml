type index = Index of int;;
type array_minimum = {minimum : int; index : int};;

let read a index =
  match index with
  | Index index -> a.(index);;

let inside a index =
  match index with
  | Index index -> if 0 <= index && index < Array.length a
    then
      true
    else
      false;;

let next index =
  match index with
  | Index index -> (Index (index + 1));;

let rec min_helper arr counter min =
  if counter = Array.length arr then
    min
  else
  if arr.(counter) < min.minimum then
    min_helper arr (counter + 1) {minimum = arr.(counter); index = counter}
  else
    min_helper arr (counter + 1) min;;

let min_index a =
  let minval = min_helper a 0 { minimum = a.(0); index = 0 } in
  (Index minval.index);;
