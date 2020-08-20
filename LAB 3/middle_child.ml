let middleChild x y z = 
if(x=y && y=z) then
-2
else if(x=y || y=z || z=x) then
-1
else
if((x<y && x>z)||(x>y && x<z)) then
x
else if((y>x && y<z)||(y<x && y>z)) then
y
else
z;;
let print_middle_child x y z = 
let a=(middleChild x y z) in
if(a = (-2)) then
"There are triplets"
else if(a = (-1)) then
"There are twins!"
else if (x<0 || y<0 || z<0) then
"Invalid inputs!"
else
"The age of the middle child is: "^string_of_int(a);;

