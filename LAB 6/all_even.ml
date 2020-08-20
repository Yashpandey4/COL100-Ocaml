let rec all_even l = 
match l with
|hd::tl -> if(hd mod 2 = 0) then hd::(all_even tl) else all_even tl
|[]->[];;
