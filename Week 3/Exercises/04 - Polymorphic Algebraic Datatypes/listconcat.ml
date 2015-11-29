type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty;;

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)));;

let rec to_list l =
  match l with
  | CEmpty -> []
  | CSingle x -> [x]
  | CApp (x, y) -> to_list x @ to_list y;;

let rec of_list (l : 'a list) : 'a clist =
  match l with
  | h1 :: h2 :: [] -> CApp (CSingle (h1), CApp (CSingle (h2), CEmpty))
  | h1 :: h2 :: tl -> CApp (CApp (CSingle (h1), CSingle (h2)), of_list tl)
  | hd :: [] -> CApp (CSingle (hd), CEmpty)
  | [] -> CEmpty

let append l1 l2 =
  match (l1, l2) with
  | (CEmpty, l) | (l, CEmpty) -> l
  | (l1, l2) -> CApp (l1, l2);;

(* let rec hdr l = *)
(*   match l with *)
(*   | CApp (x, y) -> *)
(*     (match (x, y) with *)
(*      | ) *)
(*   | CSingle x -> Some x *)
(*   | CEmpty -> None;; *)

let hd l =
  let listform = to_list l in
  if listform = [] then
    None
  else
    Some (List.nth listform 0);;

(* let rec tl l = *)
(*   match l with *)
(*   | CEmpty -> None *)
(*   | CSingle _ -> None *)
(*   | CApp (x, y) -> *)
(*     (match (x, y) with *)
(*      | ());; *)
