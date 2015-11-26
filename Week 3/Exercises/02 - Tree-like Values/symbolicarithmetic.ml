type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp;;

let my_example =
  EAdd ((EMul (EInt 2, EInt 2)), (EMul (EInt 3, EInt 3)));;

let rec eval e =
  match e with
  | EInt e -> e
  | EAdd (x, y) -> (eval x) + (eval y)
  | EMul (x, y) -> (eval x) * (eval y);;

let factorize e =
  match e with
  | EAdd (EMul (a1, b), EMul (a2, c)) ->
      if a1 = a2 then
        EMul (a1, EAdd (b, c))
      else
        e
  | e -> e;;

let expand e =
  "Replace this string with your implementation." ;;

let simplify e =
  "Replace this string with your implementation." ;;
