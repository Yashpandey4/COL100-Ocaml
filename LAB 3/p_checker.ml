(*let rec is_divisor d n =
if(d*d>n) then
true
else if(n mod d = 0) then
false
else
is_divisor (d+1) n;;
proposed algo 1 - not what i ended up writing.
let rec is_Prime n =
let n = abs n in
let rec is_not_divisor d =
d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
n <> 1 && is_not_divisor 2;;
*)

let rec is_prime n =
let rec is_divisor d n =
if(d*d>n) then
true
else if(n mod d = 0) then
false
else
is_divisor (d+1) n in

if(n=1) then
false
else
is_divisor 2 n;;

