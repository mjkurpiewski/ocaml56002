exception Empty;;

(* For development *)
let oner = ref 1;;
let twor = ref 2;;
let list1 = ref [1; 2; 3; 4; 5];;
let list2 = ref [2; 3; 4; 5; 6];;
let emptyl = ref [];;

let swap (ra : 'a ref) (rb : 'a ref) : unit =
  let a_value = !ra in
  ra := !rb;
  rb := a_value;;

let update (r : 'a ref) (f : 'a -> 'a) : 'a =
  let old_value = !r in
  r := f old_value;
  old_value;;

let move (l1 : 'a list ref) (l2 : 'a list ref) : unit =
  if !l1 = [] then
    raise Empty
  else
    let first_element = List.nth !l1 0 in
    (l1 := List.tl !l1;
     l2 := first_element :: !l2);;

let rec print_list l =
  match l with
  | [] -> print_newline
  | hd :: tl ->
    begin
      print_int hd;
      print_string " ";
      print_list tl;
    end;;

let reverse (l : 'a list) : 'a list =
  let l' = ref l in
  let reversed = ref [] in
  try
    while true do
      move l' reversed
    done;
    !reversed;
  with
  | Empty -> !reversed;;
