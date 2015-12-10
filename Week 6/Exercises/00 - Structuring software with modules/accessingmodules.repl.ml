let bfs t =
  let rec aux results = function
    | [] ->
        List.rev results
    | l :: ls ->
        let results = (focus l) :: results in
        try
          aux results (ls @ [ go_first l; go_second l])
        with Fail ->
          aux results ls
  in
  aux [] [Loc (t, Top)]
