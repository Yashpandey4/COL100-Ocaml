let rec all_even l = 
match l with
|hd::tl -> if(hd mod 2 = 0) then hd::(all_even tl) else all_even tl
|[]->[];;

let rec sum_even l = 
match l with
|hd::tl -> if(hd mod 2 = 0) then hd+(sum_even tl) else sum_even tl
|[]->0;;

let rec sum_odd l = 
match l with
|hd::tl -> if(hd mod 2 <> 0) then hd+(sum_odd tl) else sum_odd tl
|[]->0;;

let rec even_odd l = sum_even l = sum_odd l;;

let rec add_num l r =
let rec aux l n = 
match l with
|[]->[]
|hd::tl -> (hd+(r*n))::aux tl (n+1) in
aux l 1;;

exception List_Sorting_Error of string;;
let rec sort lst =
match lst with
[] -> []
| head :: tail -> insert head (sort tail)
and insert elt lst =
match lst with
[] -> [elt]
| head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail;;
let rec merge_list (l1,l2) = 
if(List.rev (sort (l1)) = l1 && List.rev(sort l2) = l2) then sort (l1@l2)
else raise (List_Sorting_Error "Input lists are not sorted!");;

let rec sort lst =
match lst with
[] -> []
| head :: tail -> insert head (sort tail)
and insert (a,b) lst =
match lst with
[] -> [(a,b)]
| (a1,b1) :: tail -> if b <= b1 then (a,b) :: lst else (a1,b1) :: insert (a,b) tail;;


let num_distinct l =
let rec aux count acc l = 
match l with
| [] -> [] 
| [x] -> (x,count+1) :: acc
| a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
else aux 0 ((a,count+1) :: acc) t in
List.rev (sort (aux 0 [] (List.rev l)));;

exception Not_a_Matrix of string;;
exception Non_matching_dimensions of string;;

let sum2 a x y = a @ [x + y];;
let sum1 a l1 l2 = if(List.length l1 <> List.length l2) then raise (Not_a_Matrix "You have not given a valid matrix")
else a @ [(List.fold_left2 sum2 [] l1 l2)];;
let matrix_add l1 l2 = 
if(l1=[] || l2=[]) then raise (Not_a_Matrix "You have not given a valid matrix")
else if(List.length l1 <> List.length l2) then raise (Non_matching_dimensions "Dimensions of the two matrices do not match")

else List.fold_left2 sum1 [] l1 l2;;

exception Not_a_Matrix of string;;
exception Non_matching_dimensions of string;;

let rec check l = 
match l with
|hd::hd1::tl -> if(List.length hd = List.length hd1) then check (hd1::tl) 
else false
|[x] -> true
|[]->true;;

let rec mapn f lists =
assert (lists <> []);
if List.mem [] lists then
[]
else
f (List.map List.hd lists) :: mapn f (List.map List.tl lists);;

let matrix_mult m1 m2 =
if(m1=[] || m2=[]) then raise (Not_a_Matrix "You have not given a valid matrix")

else if(List.length (List.nth m1 0) <> List.length m2) then
raise (Non_matching_dimensions "Dimensions of the two matrices not suitable for multiplication")
else if(check m1 = false || check m2 = false) then raise (Not_a_Matrix "You have not given a valid matrix")
else 
List.map
(fun row ->
mapn
(fun column ->
List.fold_left (+) 0
(List.map2 ( * ) row column))
m2)
m1;;
