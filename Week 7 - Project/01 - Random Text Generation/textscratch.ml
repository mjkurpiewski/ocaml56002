let words (str : string) : string list =

  let explode (s : string) : char list =
    let rec explode_aux (i : int) (acc : char list) =
      if i < 0 then
        acc
      else
        explode_aux (i - 1) (s.[i] :: acc) in
    explode_aux (String.length s - 1) [] in

  let w_buffer = Buffer.create 16 in

  let rec aux (cl : char list) (acc : string list) : string list =
    match cl with
    | [] ->
      let to_add = Buffer.contents w_buffer in
      to_add :: acc
    | ' ' :: tl ->
      let to_add = Buffer.contents w_buffer in
      (Buffer.clear w_buffer; aux tl (to_add :: acc))
    | c :: tl ->
      (Buffer.add_char w_buffer c; aux tl acc)
  in
  List.rev (aux (explode str) []);;

List.assoc "bar";;
