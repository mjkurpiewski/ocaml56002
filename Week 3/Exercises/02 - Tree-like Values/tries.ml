type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list;;

let empty = Trie (None, []);;

let example =
  Trie (None,
        [('i', Trie (Some 11,
                     [('n', Trie (Some 5,
                                  [('n', Trie (Some 9, []))]))]));
         ('t',
          Trie (None,
                [('e',
                  Trie (None,
                        [('n', Trie (Some 12, []));
                         ('d', Trie (Some 4, []));
                         ('a', Trie (Some 3, []))]));
                 ('o', Trie (Some 7, []))]));
         ('A', Trie (Some 15, []))]);;

let char_list =
  [('i', Trie (Some 11,
               [('n', Trie (Some 5,
                            [('n', Trie (Some 9, []))]))]));
   ('t',
    Trie (None,
          [('e',
            Trie (None,
                  [('n', Trie (Some 12, []));
                   ('d', Trie (Some 4, []));
                   ('a', Trie (Some 3, []))]));
           ('o', Trie (Some 7, []))]));
   ('A', Trie (Some 15, []))];;

let rec children_from_char m c =
  match m with
  | [] -> None
  | [(ch, tr)] ->
    if ch = c then
      Some tr
    else
      None
  | (ch, tr) :: tl  ->
    if ch = c then
      Some tr
    else
      children_from_char tl c;;


let rec update_children m c t =
  match m with
  | [] -> []
  | [(ch, tr)] ->
    if ch = c then
      [(ch, t)] @ update_children [] c t
    else
      [(ch, tr)] @ [(c, t)] @ update_children [] c t
  | (ch, tr) :: tl ->
    if ch = c then
      (ch, t) :: tl
    else
      [(ch, tr)] @ update_children tl c t;;

let lookup trie w =
  let rec lookup_helper trie counter =
    match trie with
    | trie when trie = empty -> None
    | Trie (opt, rest) ->
      if counter = String.length w then
        opt
      else
        let node = children_from_char rest w.[counter] in
        match node with
        | None -> None
        | Some t -> lookup_helper t (counter + 1)
  in
  lookup_helper trie 0;;

(* let insert trie (w : string) (v : int) = *)
(*   let wlen = String.length w in *)

(*   let rec insert_helper trie counter = *)
(*     match trie with *)
(*       | Trie (opt, []) -> *)
(*         if counter = (wlen - 1) then *)
(*           insert_helper (Trie (Some v, update_children [] w.[counter] empty)) counter *)
(*         else *)
(*           insert_helper (Trie (opt, update_children [] w.[counter] empty)) counter *)
(*       | Trie (opt, rest) -> *)
(*         let exists = children_from_char rest w.[counter] in *)
(*         match exists with *)
(*         | None -> insert_helper (Trie (opt, update_children rest w.[counter] empty)) counter *)
(*         | Some t -> insert_helper t (counter + 1) *)
(*   in *)
(*   if lookup trie w = Some v then *)
(*     trie *)
(*   else *)
(*     insert_helper trie 0;; *)
