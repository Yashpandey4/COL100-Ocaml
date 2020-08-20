let rec swap x i j = 
  let arr = Array.of_list x in
  let a = arr.(i) in
  let b = arr.(j) in
    arr.(i)<-b;
    arr.(j)<-a;
    Array.to_list arr;;

let mult x i c = 
  let aux n = n*.c in
  let a = List.map aux (List.nth x i) in
  let arr = Array.of_list x in

    arr.(i)<-a;

    Array.to_list arr;;

let sum2 a x y = a @ [x -. y];;
let sum1 a l1 l2 = a @ (List.fold_left2 sum2 [] l1 l2);;


let addRows x i j =
  let sum2 a x y = a @ [x +. y] in 
  let sum1 a l1 l2 =  a @ (List.fold_left2 sum2 [] l1 l2) in

  let a = sum1 [] (List.nth x i) (List.nth x j) in
  let arr = Array.of_list x in

    arr.(i)<-a;

    Array.to_list arr;;

let rec transpose m =
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transpose (List.map List.tl m);;

let zerocolumn x = 
  let rec zerocheck n = n<>0. in
  let rec aux i = 
    if(i<(List.length x)) then
      if (List.exists zerocheck (List.nth x i)) then i
      else aux (i+1) 
    else -1 in
    aux 0;;

let firstzero x =
  let rec aux i = 
    if(i<(List.length x)) then
      if ((List.nth x i)<>0.) then i
      else aux (i+1) 
    else -1 in
    aux 0;;

let rec internalarray x = 
  let rec aux n =
    if(n<List.length x) then
      Array.of_list(List.nth x n)::aux (n+1)
    else [] in
    aux 0;;

let rec internallist x = 
  let rec aux n =
    if(n<Array.length x) then
      Array.to_list(x.(n))::aux (n+1)
    else [] in
    aux 0;;

let rec rowEchelon x = 
  let m = List.length x in
    if(m=1) then x else
      let tx = transpose x in
      let c = zerocolumn tx in
      let i = firstzero (List.nth tx c) in

      let sx = swap x 0 i in
      let rec stepv j sx1= 
        if(j<m) then
          let sx2 = internalarray sx1 in
          let arr = Array.of_list sx2 in
          let a1 = mult sx1 0 ((arr.(j).(c))/.(arr.(0).(c))) in
          let a3 = sum1 [] (List.nth sx1 j) (List.nth a1 0) in

            arr.(j)<-Array.of_list a3;
            stepv (j+1) (internallist arr)
        else sx1 in

      let hd::tl = (stepv 1 sx) in
        hd :: (rowEchelon tl);;

















