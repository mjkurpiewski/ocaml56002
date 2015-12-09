let rec print_int_list (l : int list) : unit =
  match l with
  | [] -> print_newline ()
  | hd :: [] -> print_int hd; print_int_list []
  | hd :: tl -> print_int hd; print_newline (); print_int_list tl;;

let print_every_other k l =
  List.iteri (fun i element ->
      if i mod k <> 0 then
        ()
      else
        print_int element; print_newline ()) l;;

let rec print_list print l =
    match l with
  | [] -> print_newline ()
  | hd :: [] -> print hd; print_list print [];
  | hd :: tl -> print hd; print_newline (); print_list print tl;;
