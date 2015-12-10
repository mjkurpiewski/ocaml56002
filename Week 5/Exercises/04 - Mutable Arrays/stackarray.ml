type stack = int array;;
exception Full;;
exception Empty;;

let create (size : int) : stack =
  Array.init
    (size + 1)
    (fun _ -> 0);;

let push (buffer : stack) (element : int) : unit =
  let
    num_elements = buffer.(0) and
    last_element = Array.length buffer - 1 in
  if num_elements = last_element then
    raise Full
  else
    (buffer.(num_elements + 1) <- element;
     buffer.(0) <- num_elements + 1);;

let append (buffer : stack) (elements : int array) : unit =
  try
    for i = Array.length elements - 1 downto 0 do
      push buffer elements.(i)
    done
  with
  | Full -> raise Full;;

let pop (buffer : stack) : int =
  let num_elements = buffer.(0) in
  if num_elements = 0 then
    raise Empty
  else
    let to_return = buffer.(num_elements) in
    (buffer.(0) <- num_elements - 1;
     buffer.(num_elements) <- 0;
     to_return);;
