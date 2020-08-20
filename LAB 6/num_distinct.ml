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