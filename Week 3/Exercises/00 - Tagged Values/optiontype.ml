let find a w =
  let rec find_helper strs counter =
    if counter = Array.length strs then
      None
    else if strs.(counter) = w then
      Some counter
    else
      find_helper strs (counter + 1) in
  find_helper a 0;;

let default_int = function
  | None -> 0
  | Some x -> x;;

let merge a b =
  match (a, b) with
  | (None, None) -> None
  | (Some a, Some b) -> Some (a + b)
  | (Some a, _) -> Some a
  | (_, Some b) -> Some b;;
