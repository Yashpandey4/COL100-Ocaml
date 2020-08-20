
let rec help n k =
if (n=1) then
0
else
((help (n-1) k)+k) mod (n);;

let rec josephus n k x= 
let a = ((x+(help n k)) mod (n)) in
if(a=0) then
n
else
a;;

