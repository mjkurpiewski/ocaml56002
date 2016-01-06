(*
multiple_of that takes two integer parameters, n and d, and determines whether n is a multiple of d. The function must return a boolean value. This function can be written without recursion. Look at the operators defined on integers in sequence 1.
*)

let multiple_of n d =
  if n mod d == 0 then
    true
  else
    false;;

(*
integer_square_root that calculates the integer square root of a positive integer n, that is the largest integer r such that r * r <= n. Hint: you may use floating point arithmetic, but don't forget that you have to convert explicitely between float and int.
*)

let integer_square_root n =
  int_of_float (sqrt(float_of_int n));;
