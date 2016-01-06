let is_multiple i x = i mod x = 0;;

let output_multiples x n m =
  for count = n to m do
    if is_multiple count x then
      (print_int count; print_string ", "; ())
  done;;

exception Zero_found of string;;
let zeroexception : exn = Zero_found ("zero");;

let display_sign_until_zero f m =
  try
    for count = 0 to m do
      if f count > 0 then
        (print_string "positive"; print_newline ())
      else if f count < 0 then
        (print_string "negative"; print_newline ())
      else if f count = 0 then
        raise zeroexception
    done
  with
  | Zero_found (msg) -> (print_string msg; ());;

