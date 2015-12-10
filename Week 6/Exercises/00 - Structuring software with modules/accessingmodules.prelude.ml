module Tree = struct

  type 'a t = Leaf of 'a | Node of 'a t * 'a * 'a t

  module Iterator = struct

    type 'a path =
      | Top
      | Left of 'a path * 'a * 'a t
      | Right of 'a t * 'a * 'a path

    type 'a iterator = Loc of 'a t * 'a path

    exception Fail

    let go_left (Loc (t, p)) =
      match p with
	| Top -> raise Fail
	| Left (father, x, right) -> raise Fail
	| Right (left, x, father) -> Loc (left, Left (father, x, t))

    let go_right (Loc (t, p)) =
      match p with
      | Top -> raise Fail
      | Left (father, x, right) -> Loc (right, Right (t, x, father))
      | Right (left, x, father) -> raise Fail

    let go_up (Loc (t, p)) =
      match p with
	| Top -> raise Fail
	| Left(father, x, right) -> Loc (Node (t, x, right), father)
	| Right(left, x, father) -> Loc (Node (left, x, t), father)

    let go_first (Loc (t, p)) =
      match t with
	| Leaf _ -> raise Fail
	| Node (left, x, right) -> Loc (left, Left (p, x, right))

    let go_second (Loc (t, p)) =
      match t with
	| Leaf _ -> raise Fail
	| Node (left, x, right) -> Loc (right, Right (left, x, p))

    let focus (Loc ((Leaf x | Node (_, x, _)), _)) = x

  end

end