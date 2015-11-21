type point = {x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move p dp =
  {x = p.x +. dp.dx; y = p.y +. dp.dy; z = p.z +. dp.dz};;

let next obj =
  let oldpoint = obj.position in
  let velocity = obj.velocity in
  {position = move oldpoint velocity; velocity = velocity};;

let will_collide_soon p1 p2 =
  let nextp1 = next p1 and nextp2 = next p2 in
  let p1pos = nextp1.position and p2pos = nextp2.position in
  if (sqrt ((p1pos.x -. p2pos.x) ** 2.) +.
      ((p1pos.y -. p2pos.y) ** 2.) +.
      ((p1pos.z -. p2pos.z) ** 2.)) < 2.0 then
    true
  else
    false;;
