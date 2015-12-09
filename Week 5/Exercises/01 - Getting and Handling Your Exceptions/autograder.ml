type report = message list
and message = string * status
and status = Successful | Failed;;

type 'a result = Ok of 'a | Error of exn;;

let exec f x =
  try
    let res = f x in
    Ok (res)
  with
  | except -> Error (except);;

let compare user reference to_string : message =
  match user, reference with
  | Ok (u), Ok (r) ->
    if u = r then
      ("got correct value " ^ to_string u, Successful)
    else
      ("got unexpected value " ^ to_string u, Failed)
  | Ok (u), Error (r) -> ("got unexpected value " ^ to_string u, Failed)
  | Error (u), Ok (r) -> ("got unexpected exception " ^ exn_to_string u, Failed)
  | Error (u), Error (r) ->
    if u = r then
      ("got correct exception " ^ exn_to_string u, Successful)
    else
      ("got unexpected exception " ^ exn_to_string u, Failed);;

let test user reference sample to_string : report =
  let rec aux counter new_report =
    if counter = 10 then
      new_report
    else
      let rng = sample () in
      let user_result = exec user rng and ref_result = exec reference rng in
      let new_message = compare user_result ref_result to_string in
      aux (counter + 1) (List.append new_report [new_message]) in
  aux 0 [];;
