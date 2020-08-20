let matrix_multiply x y =
  let x0 = Array.length x
  and y0 = Array.length y in
  let y1 = if y0 = 0 then 0 else Array.length y.(0) in
  let z = Array.make_matrix x0 y1 0 in
  for i = 0 to x0-1 do
    for j = 0 to y1-1 do
      for k = 0 to y0-1 do
        z.(i).(j) <- z.(i).(j) + x.(i).(k) * y.(k).(j)
      done
    done
  done;
  z;;

let rec mapn f lists =
  assert (lists <> []);
  if List.mem [] lists then
    []
  else
    f (List.map List.hd lists) :: mapn f (List.map List.tl lists)
 
let matrix_multiply m1 m2 =
  List.map
    (fun row ->
      mapn
       (fun column ->
         List.fold_left (+) 0
          (List.map2 ( * ) row column))
       m2)
    m1
