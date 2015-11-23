(* Devel/Debug code *)
let somearray = Array.make 20 0;;

(* Function code *)
type array_minimum = {minimum : int; index : int};;

let rec min_helper arr counter min =
    if counter = Array.length arr then
      min
    else
    if arr.(counter) < min.minimum then
      min_helper arr (counter + 1) {minimum = arr.(counter); index = counter}
    else
      min_helper arr (counter + 1) min;;

let min a =
  (min_helper a 0 {minimum = a.(0); index = 0}).minimum;;

let min_index a =
  (min_helper a 0 {minimum = a.(0); index = 0}).index;;

let it_scales = "no";;
