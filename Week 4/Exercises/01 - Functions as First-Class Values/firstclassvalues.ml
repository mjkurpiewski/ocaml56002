let rec compose = function
  | [] -> fun x -> x
  | hd :: tl -> fun x -> hd ((compose tl) x);;

let rec fixedpoint (f : float -> float)
    (start : float) (delta : float) : float =
  if abs_float (start -. f start) < delta then
    start
  else
    fixedpoint f (f start) delta;;
