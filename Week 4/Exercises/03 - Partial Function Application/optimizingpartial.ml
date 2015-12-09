let ccr =
  fun a ->
    let acos = 8. *. cos (a /. 2.)
    in fun b ->
      let bcos = acos *. cos (b /. 2.)
      in fun c ->
        let ccos = bcos *. cos (c /. 2.)
        in fun s ->
          s /. ccos;;
