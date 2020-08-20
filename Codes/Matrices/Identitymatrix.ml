let make_id_matrix n =
    let m = Array.make_matrix n n 0.0 in
    for i = 0 to pred n do
      m.(i).(i) <- 1.0
    done;
    (m)
  ;;



let make_id_matrix n =
    Array.init n (fun i ->
      Array.init n (fun j ->
        if i = j then 1.0 else 0.0))
  ;;
