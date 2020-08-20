let next_dir = function
  |  1,  0 ->  0, -1
  |  0,  1 ->  1,  0
  | -1,  0 ->  0,  1
  |  0, -1 -> -1,  0
  | _ -> assert false
 
let next_pos ~pos:(x,y) ~dir:(nx,ny) = (x+nx, y+ny)
 
let next_cell ar ~pos:(x,y) ~dir:(nx,ny) =
  try ar.(x+nx).(y+ny)
  with _ -> -2
 
let for_loop n init fn =
  let rec aux i v =
    if i < n then aux (i+1) (fn i v)
  in
  aux 0 init
 
let spiral ~n =
  let ar = Array.make_matrix n n (-1) in
  let pos = 0, 0 in
  let dir = 0, 1 in
  let set (x, y) i = ar.(x).(y) <- i in
  let step (pos, dir) =
    match next_cell ar pos dir with
    | -1 -> (next_pos pos dir, dir)
    | _ -> let dir = next_dir dir in (next_pos pos dir, dir)
  in
  for_loop (n*n) (pos, dir)
           (fun i (pos, dir) -> set pos i; step (pos, dir));
  (ar)
 
let print =
  Array.iter (fun line ->
    Array.iter (Printf.printf " %2d") line;
    print_newline())
 
let () = print(spiral 5)

(*Another implementation:*)

let spiral n =
   let ar = Array.make_matrix n n (-1) in
   let out i = i < 0 || i >= n in
   let too_far (x,y) = out x || out y || ar.(x).(y) >= 0 in
   let step x y (dx,dy) = (x+dx,y+dy) in
   let turn (i,j) = (j,-i) in
   let rec iter (x,y) d i =
      ar.(x).(y) <- i;
      if i < n*n-1 then
         let d' = if too_far (step x y d) then turn d else d in
         iter (step x y d') d' (i+1) in
   (iter (0,0) (0,1) 0; ar)
 
let show =
   Array.iter (fun v -> Array.iter (Printf.printf " %2d") v; print_newline())
 
let _ = show (spiral 5)
