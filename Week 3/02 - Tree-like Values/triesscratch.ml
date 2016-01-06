let samp = [('p', Trie (None, [('m', Trie (None, [('d', Trie (Some 3, []))]))]));
            ('d', Trie (None, [('m', Trie (Some (-1), []))]));
            ('j', Trie (Some 2, [('g', Trie (None, [('g', Trie (Some 1, []))]))]))];;

let rep = (Trie (Some 2,
                 [('j', Trie (None, [('a', Trie (None, [('g', Trie (Some 0, []))]))]));
                  ('p',
                   Trie (None,
                         [('j', Trie (None, [('d', Trie (Some (-4), []))]));
                          ('g', Trie (Some 0, []))]));
                  ('a', Trie (None, [('p', Trie (Some 0, []))]))]));;

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

update_children
  [('j', Trie (None, [('d', Trie (Some 3, []))]));
   ('m', Trie (None, [('j', Trie (Some (-2), []))]))]
  'p'
  (Trie (Some 2,
    [('p', Trie (None, [('g', Trie (None, [('m', Trie (Some (-2), []))]))]));
     ('g', Trie (None, [('m', Trie (None, [('p', Trie (Some (-2), []))]))]));
     ('a', Trie (None, [('d', Trie (Some 0, []))]));
     ('j', Trie (None, [('j', Trie (Some (-1), []))]))]));;

    match to_update with
    | Trie (opt, subtrie) ->
      if insert_length = 0 then
        Trie(Some insert_val, subtrie)
      else if insert_length = idx then
        to_update
      else
        let next_character = to_insert.[idx] in
        match (children_from_char subtrie next_character) with
        | None ->
          if insert_length = (idx + 1) then
            insert_helper (Trie (opt, (update_children subtrie next_character leaf))) (idx + 1)
          else
            insert_helper (Trie (opt, (update_children subtrie next_character empty))) idx
        | Some t ->
          let (opt, subtrie) = t in
          insert_helper Trie  (idx + 1)

      if insert_length = idx then
        to_update
      else
        match to_update with
        | [] ->
          if insert_length = (idx - 1) then
            insert_helper (update_children to_update to_insert.[idx] leaf) (idx + 1)
          else
            insert_helper (update_children to_update to_insert.[idx] empty) (idx + 1)
        | (some_char, subtrie) :: tl ->
          if some_char = to_insert.[idx] then
            let Trie (next_opt, next_ctc) = subtrie in
            Trie (opt, insert_helper next_ctc (idx + 1))
          else
            insert_helper tl idx
    in

      if to_insert = "" then
    let Trie (opt, ctc) = to_update in
    Trie (Some insert_val, ctc)
  else
    let insert_length = String.length to_insert in
    let leaf = Trie (Some insert_val, []) in

    let rec insert_helper (to_update : trie) (idx : int) : trie =
      let rec list_scanner
          (to_search : char_to_children)
          (trie_option : int option)
          (idx : int) : char_to_children =
        match to_search with
        | [] ->
          if insert_length = idx then
            Trie (trie_option, (update_children to_search to_insert.[idx] leaf))
          else
            Trie (trie_option, (update_children to_search to_insert.[idx] empty))
        | (some_char, subtrie) :: tl ->
          if some_char = to_insert.[idx] then
            insert_helper subtrie (idx + 1)
          else
            list_scanner tl trie_option idx
      in
      match to_update with
      | Trie (opt, []) -> to_update
      | Trie (opt, child_list) ->
        Trie (opt, (list_scanner child_list opt idx))
