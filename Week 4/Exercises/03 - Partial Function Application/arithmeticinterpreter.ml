type operation =
    Op of string * operation * operation
  | Value of int;;

type env = (string * (int -> int -> int)) list;;

let rec lookup_function n = function
  | [] -> invalid_arg "lookup_function"
  | [(fname, procedure)] ->
    if n = fname then
      procedure
    else
      lookup_function n []
  | (fname, procedure) :: tl ->
    if n = fname then
      procedure
    else
      lookup_function n tl;;

let add_function (name : string) (op : (int -> int -> int)) (env : env) =
  List.append env [(name, op)];;

let my_env =
  add_function "min" (fun a -> fun b -> if a < b then a else b) initial_env;;

let rec compute env op =
  match op with
  | Value x -> x
  | Op (n, o1, o2) ->
    let f = lookup_function n env in
     f (compute env o1) (compute env o2);;

let rec compute_eff env =
  fun op ->
    match op with
    | Value x -> x
    | Op (n, o1, o2) ->
      (lookup_function n env) (compute_eff env o1) (compute_eff env o2);;
