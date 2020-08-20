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

let addRows2 x y = 
let sum2 a x y = a @ [x +. y] in 
let sum1 a l1 l2 =  a @ (List.fold_left2 sum2 [] l1 l2) in
sum1 [] x y ;;

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


let rec rowEchelon x = 
let m = List.length x in
if(m)=1 then x else
let tx = transpose x in
let c = zerocolumn x in
let i = List.nth tx c in
let arr = Array.of_list x in

let rec stepv j x = 
if(j<m) then
let arr = Array.of_list x in
let a1 = mult x 0 ((List.nth arr.(j) c)/.(List.nth arr.(0) c)) in
let a2 = mult a1 0 (-1.) in
let a3 = addRows2 (List.nth x j) (List.nth a2 0) in

arr.(j)<-a3;
stepv (j+1) (Array.to_list arr)
else x in

let a4 = stepv 0 x in
let hd::tl = a4 in
(List.nth (a4) 0):: rowEchelon tl;;















