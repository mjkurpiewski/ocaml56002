(*
Write a function exchange of type int -> int that takes an integer x between 10 and 99 and returns an integer which is x whose digits have been exchanged. For instance, exchange 73 = 37.
*)

let exchange (x : int) : int =
  if (x >= 10) && (x <= 99) then
  let strform (x : int) : string =
    string_of_int x in
  let reversed (s : string) : int =
    int_of_string ((String.get strform 1) ^ (String.get strform 0)) in
else
  -1;;



