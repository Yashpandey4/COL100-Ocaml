let rec square_root a k =
if (k=1) then
(1.0+.a)/.2.0
else if (k>1) then 
let x = square_root a (k-1) in
(x+.(a/.x))/.2.0
else
sqrt a;;