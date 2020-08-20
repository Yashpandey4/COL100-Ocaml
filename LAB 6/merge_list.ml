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
