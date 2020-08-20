let checkDimension a b = 

let rec check l = 
match l with
|hd::hd1::tl -> if(List.length hd = List.length hd1) then check (hd1::tl) 
else false
|[x] -> true
|[]->true in

(check a) && ((List.length a)=(List.length b));;
