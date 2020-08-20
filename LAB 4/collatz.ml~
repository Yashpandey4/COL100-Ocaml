let rec collatz n = 
 let rec count_collatz n count =
if(n=1) then
  count
else if(n mod 2 = 0) then
count_collatz (n/2) (count+1)
  else
count_collatz ((3*n)+1) (count+1) in
count_collatz n 0;;

let rec max_collatz n=
if(n=1) then
1
else 
let k=max_collatz (n-1) in
if (collatz k > collatz n) then k
else n;;



