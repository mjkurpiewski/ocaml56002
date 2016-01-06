(* The given prelude... *)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list;;

let empty = Trie (None, []);;

let example =
  Trie (None,
  [('i', Trie (Some 11,
              [('n', Trie (Some 5,
                           [('n', Trie
                               (Some 9, []))]))]));
   ('t',
    Trie (None,
    [('e',
      Trie (None,
          [('n', Trie (Some 12, []));
        ('d', Trie (Some 4, []));
        ('a', Trie (Some 3, []))]));
   ('o', Trie (Some 7, []))]));
  ('A', Trie (Some 15, []))]);;

(* Written code *)

let rec children_from_char
    (input : char_to_children)
    (query : char) : trie option =
  match input with
  | [] -> None
  | (some_char, subtrie) :: tl ->
    if some_char = query then
      Some subtrie
    else
      children_from_char tl query;;

let rec update_children
    (child_list : char_to_children)
    (target : char)
    (structure : trie) : char_to_children =
  match child_list with
  | [] -> [(target, structure)]
  | (some_char, subtrie) :: tl ->
    if some_char = target then
      (some_char, structure) :: tl
    else
     [(some_char, subtrie)] @ update_children tl target structure;;

let lookup (to_search : trie) (target : string) : int option =
  let target_length = String.length target in

  let rec lookup_helper (to_search : trie) (idx : int) : int option =
    match to_search with
    | Trie (opt, subtrie) ->
      if idx = target_length then
        opt
      else
        let this_node = children_from_char subtrie target.[idx] in
        match this_node with
        | None -> None
        | Some x -> lookup_helper x (idx + 1)
  in
  lookup_helper to_search 0;;

let insert
    (to_update : trie)
    (to_insert : string)
    (insert_val : int) : trie =

  let rec insert_helper (to_update : trie option) (idx : int) : trie =
    match to_update with
    | None ->
    | Some t -> let (opt, chars) = t in
      insert_helper (children_from_char chars to_insert.[idx]) (idx + 1)

    in
    match (lookup to_update to_insert) with
    | Some x when x = insert_val -> to_update
    | Some x -> insert_helper to_update 0
    | None -> insert_helper to_update 0;;
