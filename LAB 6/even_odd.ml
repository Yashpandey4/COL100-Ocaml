let rec sum_even l = 
match l with
|hd::tl -> if(hd mod 2 = 0) then hd+(sum_even tl) else sum_even tl
|[]->0;;

let rec sum_odd l = 
match l with
|hd::tl -> if(hd mod 2 <> 0) then hd+(sum_odd tl) else sum_odd tl
|[]->0;;

let rec even_odd l = sum_even l = sum_odd l;;
