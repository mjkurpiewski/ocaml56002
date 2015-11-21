(*
The Given Prelude
*)
type point2d = int * int;;
type tetragon = point2d * point2d * point2d * point2d;;

(*
Development/scratch
*)

let lup : point2d = (5, 0);;
let rup : point2d = (5, 5);;
let llp : point2d = (0, 0);;
let rlp : point2d = (0, 5);;
let bad : point2d = (7, 5);;

let awellformedtetragon : tetragon = (lup, rup, llp, rlp);;
let apoorlyformedtetragon : tetragon = (lup, rup, llp, bad);;

(*
Actual code
*)

let xcoord (x, _) = x;;
let ycoord (_, y) = y;;

let rotate_point (x, y) : point2d =
  (y, -x);;

let rec reorder (p1, p2, p3, p4) : tetragon =
  let lup (lup, _, _, _) = lup in
  let rup (_, rup, _, _) = rup in
  let llp (_, _, llp, _) = llp in
  let rlp (_, _, _, rlp) = rlp in
  let reordered:tetragon = (p1, p2, p3, p4) in
  if xcoord (lup reordered) > xcoord (rup reordered) then
    reorder (p2, p1, p3, p4)
  else
  if xcoord (llp reordered) > xcoord (rlp reordered) then
    reorder (p1, p2, p4, p3)
  else
  if ycoord (lup reordered) < ycoord (llp reordered) then
    reorder (p3, p2, p1, p4)
  else
  if ycoord (rup reordered) < ycoord (rlp reordered) then
    reorder (p1, p4, p3, p2)
  else
    reordered;;

let pairwise_distinct (lup, rup, llp, rlp) : bool =
  if lup <> rup &&
     llp <> rlp &&
     lup <> llp &&
     rup <> rlp then
  true
else
  false;;

let wellformed (lup, rup, llp, rlp) : bool =
  if xcoord lup < xcoord rup &&
     xcoord llp < xcoord rlp &&
     ycoord lup > ycoord llp &&
     ycoord rup > ycoord rlp then
    true
  else
    false;;

let rotate_tetragon (lup, rup, llp, rlp) : tetragon =
  reorder ((rotate_point lup),
           (rotate_point rup),
           (rotate_point llp),
           (rotate_point rlp));;
