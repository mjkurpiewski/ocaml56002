let exchange (x : int) : int =
  if (x >= 10) && (x <= 99) then
    let intstring = string_of_int x in
    let revstring = Char.escaped (String.get intstring 1) ^ Char.escaped (String.get intstring 0) in
    int_of_string revstring
  else
    -1;;

let is_valid_answer (grand_father_age, grand_son_age) : bool =
  if (grand_father_age = 4 * grand_son_age) && (exchange grand_son_age = 3 * exchange grand_father_age) then
    true
  else
    false;;

let rec find (max_grand_father_age, min_grand_son_age) : (int * int) =
  let rec range n m =
    if n > m then
      []
    else
      n :: (range (n + 1) m) in
  let defined_range =
    range 1 max_grand_father_age in
  let grand_father_candidates =
    List.map (fun x -> if x mod 4 = 0 && (exchange x) mod 3 = 0 then x else 0) defined_range in
  let filtered_candidates =
    List.filter (fun x -> x > 0) grand_father_candidates in


(*
let thislist =
  List .map (Core.Std.List.range 0 100)
    (fun x -> if x mod 4 = 0 && (exchange x) mod 3 = 0 then x else 0)
*)
