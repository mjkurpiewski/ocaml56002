type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist;;

let nil () =
  { pointer = Nil };;

let cons elt rest =
  { pointer = List (elt, rest) };;

exception Empty_xlist;;

let head (l : 'a xlist) : 'a =
  match l with
  | { pointer = List (element, tl) } -> element
  | nil -> raise Empty_xlist;;

let tail (l : 'a xlist) : 'a xlist =
  match l with
  | { pointer = List (element, tl) } -> tl
  | nil -> raise Empty_xlist;;

let add (a : 'a) (l : 'a xlist) : unit =
  if l.pointer = Nil then
    l.pointer <- List (a, nil ())
  else
    let rest = l.pointer in
    l.pointer <- List (a, {pointer = rest});;

let chop (l : 'a xlist) : unit =
  if l.pointer = Nil then
    raise Empty_xlist
  else
    let tl = tail l in
    l.pointer <- tl.pointer;;

let rec append (l : 'a xlist) (l' : 'a xlist) : unit =
  match l.pointer with
  | List (element, tl) -> append tl l'
  | Nil -> l.pointer <- l'.pointer;;

let rec filter (p : 'a -> bool) (l : 'a xlist) : unit =
  match l.pointer with
  | List (element, tl) ->
    if not (p element) then
      (chop l; filter p l)
    else
      filter p tl
  | Nil -> ();;
