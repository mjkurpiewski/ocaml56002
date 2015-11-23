



let pairwise_distinct (target : tetragon) : bool =
  match target with
    lup -> (lup, _, _, _) target
  | rup -> (_, rup, _, _) target
  | llp -> (_, _, llp, _) target
  | rlp -> (_, _, _, rlp) target
in
if lup <> rup &&
   llp <> rlp &&
   lup <> llp &&
   rup <> rlp then
  true
else
  false;;

let pairwise_distinct (target : tetragon) : bool =
  let lup (lup, _, _, _) = lup in
  let rup (_, rup, _, _) = rup in
  let llp (_, _, llp, _) = llp in
  let rlp (_, _, _, rlp) = rlp in
  if lup target <> rup target &&
     llp target <> rlp target &&
     lup target <> llp target &&
     rup target <> rlp target then
    true
  else
    false;;
