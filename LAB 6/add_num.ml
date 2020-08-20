let rec add_num l r =
let rec aux l n = 
match l with
|[]->[]
|hd::tl -> (hd+(r*n))::aux tl (n+1) in
aux l 1;;