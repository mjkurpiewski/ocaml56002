type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt;;

exception Unbalanced of int;;

balanced (Node 2x h
            (Node 2x h
               (Empty, h
                -4,
                Empty), h
             3,
             Node
               (Empty, h
                -1,
                Empty)));;

