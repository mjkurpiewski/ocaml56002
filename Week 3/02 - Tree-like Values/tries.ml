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
