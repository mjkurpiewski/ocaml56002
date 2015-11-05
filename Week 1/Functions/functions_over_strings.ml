(*
last_character that returns the last character of a string, assuming that the string argument is not empty;
*)

let last_character str =
  let length = String.length str in
  String.get str (length - 1);;

(*
string_of_bool that converts a boolean value to its string representation.
*)

let string_of_bool bool =
  if bool then
    "true"
  else
    "false";;
