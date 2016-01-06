(* The given prelude... *)
type image = int -> int -> bool;;

type blend =
  | Image of image
  | And of blend * blend
  | Or of blend * blend
  | Rem of blend * blend;;

let black_char = "#";;
let white_char = " ";;

let all_white =
  fun x y -> false;;
let all_black =
  fun x y -> true;;

let checkers =
  fun x y ->
    y / 2 mod 2 = x / 2 mod 2;;

let square cx cy s =
  fun x y ->
    let minx = cx - s / 2 in
    let maxx = cx + s / 2 in
    let miny = cy - s / 2 in
    let maxy = cy + s / 2 in
    x >= minx && x <= maxx && y >= miny && y <= maxy;;

let disk cx cy r =
  fun x y ->
    let x' = x - cx in
    let y' = y - cy in
    (x' * x' + y' * y') <= r * r;;

(* Exercises *)

let display_image (width : int) (height : int) (f_image : image) : unit =
  for y = 0 to height do
    for x = 0 to width do
      if f_image x y then
        print_string black_char
      else
        print_string white_char;
      if x = width then
        print_newline ();
    done
  done;;

let render (blend : blend) (x : int) (y : int) : bool =
  let rec render blend =
    match blend with
    | Image (f) -> f x y
    | And (f, g) -> render f && render g
    | Or (f, g) -> render f || render g
    | Rem (f, g) -> render f = true && render g = false
  in
  render blend;;

let display_blend (width : int) (height : int) (blend : blend) : unit =
  for y = 0 to height do
    for x = 0 to width do
      if render blend x y then
        print_string black_char
      else
        print_string white_char;
      if x = width then
        print_newline ();
    done
  done;;

