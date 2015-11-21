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

let lup (lup, _, _, _) = lup
let rup (_, rup, _, _) = rup
let llp (_, _, llp, _) = llp
let rlp (_, _, _, rlp) = rlp

let xcoord (x, _) = x;;
let ycoord (_, y) = y;;

let rotate_point (to_rotate : point2d) : point2d =
  (ycoord to_rotate, -(xcoord to_rotate));;

(* lup, rup, llp, rlp*)
let rec reorder p1 p2 p3 p4 : tetragon =
  let reordered:tetragon = (p1, p2, p3, p4) in
  if xcoord (lup reordered) > xcoord (rup reordered) then
    reorder p2 p1 p3 p4
  else
  if xcoord (llp reordered) > xcoord (rlp reordered) then
    reorder p1 p2 p4 p3
  else
  if ycoord (lup reordered) < ycoord (llp reordered) then
    reorder p3 p2 p1 p4
  else
  if ycoord (rup reordered) < ycoord (rlp reordered) then
    reorder p1 p4 p3 p2
  else
    reordered;;

let pairwise_distinct (target : tetragon) : bool =
  if lup target <> rup target &&
     llp target <> rlp target &&
     lup target <> llp target &&
     rup target <> rlp target then
  true
else
  false;;

let wellformed (target : tetragon) : bool =
  if xcoord (lup target) <= xcoord (rup target) &&
     xcoord (llp target) <= xcoord (rlp target) &&
     ycoord (lup target) >= ycoord (llp target) &&
     ycoord (rup target) >= ycoord (rlp target) then
    true
  else
    false;;

let rotate_tetragon (to_rotate : tetragon) : tetragon =
  reorder (rotate_point (lup to_rotate))
    (rotate_point (rup to_rotate))
    (rotate_point (llp to_rotate))
    (rotate_point (rlp to_rotate));;
