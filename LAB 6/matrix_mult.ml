exception Non_matching_dimensions of string;;

let rec mapn f lists =
assert (lists <> []);
if List.mem [] lists then
[]
else
f (List.map List.hd lists) :: mapn f (List.map List.tl lists)

let matrix_mult m1 m2 =
if(List.length (List.nth m1 0) = List.length m2) then
List.map
(fun row ->
mapn
(fun column ->
List.fold_left (+) 0
(List.map2 ( * ) row column))
m2)
m1
else
raise (Non_matching_dimensions "Dimensions of the two matrices not suitable for multiplication");;
