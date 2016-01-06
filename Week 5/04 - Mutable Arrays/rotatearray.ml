let rotate (a : 'a array) : unit =
  if a = [||] then () else
    let n = Array.length a in
    let v = a.(0) in
    for i = 0 to n - 2 do
      a.(i) <- a.(i+1)
    done;
    a.(n - 1) <- v;;

let rec rotate_by (a : 'a array) (n : int) : unit =
  let l = Array.length a in
  if n < 0 then
    rotate_by a (l - ((abs n) mod l));
  for i = 1 to n do
    rotate a
  done;;
