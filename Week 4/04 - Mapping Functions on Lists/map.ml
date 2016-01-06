type 'a tree =
    Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

let wrap l =
  List.map (fun e -> [e]) l;;

let rec tree_map (f : 'a -> 'b) =
  fun (t : 'a tree) ->
    match t with
    | Leaf x -> Leaf (f x)
    | Node (l, n, r) ->
      Node ((tree_map f) l, f n, (tree_map f) r);;
