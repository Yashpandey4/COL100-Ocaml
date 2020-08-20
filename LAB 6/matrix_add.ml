exception Not_a_Matrix of string;;
exception Non_matching_dimensions of string;;
let sum2 a x y = a @ [x + y];;
let sum1 a l1 l2 = if(List.length l1 <> List.length l2) then raise (Not_a_Matrix "You have not given a valid matrix")
else a @ [(List.fold_left2 sum2 [] l1 l2)];;
let matrix_add l1 l2 = if(List.length l1 <> List.length l2) then raise (Non_matching_dimensions "Dimensions of the two matrices do not match")
else List.fold_left2 sum1 [] l1 l2;;