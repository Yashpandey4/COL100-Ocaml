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

let addRows x i j =
let sum2 a x y = a @ [x +. y] in 
let sum1 a l1 l2 =  a @ (List.fold_left2 sum2 [] l1 l2) in

let a = sum1 [] (List.nth x i) (List.nth x j) in
let arr = Array.of_list x in

arr.(i)<-a;

Array.to_list arr;;

