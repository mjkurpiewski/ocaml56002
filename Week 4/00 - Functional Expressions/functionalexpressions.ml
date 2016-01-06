let rec last_element = function
  | [] -> (invalid_arg "last_element")
  | hd :: [] -> hd
  | hd :: tl -> last_element tl;;

let rec is_sorted = function
  | [] -> true
  | hd :: [] -> true
  | h1 :: h2 :: tl ->
      if h1 < h2 then
        is_sorted (h2 :: tl)
      else
        false;;
