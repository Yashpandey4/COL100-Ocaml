(* ========================================================================================== *)
let rec all_even l =
  match l with
  | [] -> []
  | h::t -> if ((h mod 2) = 0) then h::(all_even t) else (all_even t);;
(* ========================================================================================== *)



(* ========================================================================================== *)
let even_odd l =
  let rec helper l even odd =
      match l with
      | [] -> (even = odd)
      | h::t -> if ((h mod 2) = 0) then (helper t (even+h) odd) else (helper t even (odd+h))
  in
  (helper l 0 0);;
(* ========================================================================================== *)



(* ========================================================================================== *)
let add_num l k =
  let rec helper l k i =
      match l with
      | [] -> []
      | h::t -> (h + (i*k))::(helper t k (i+1))
  in
  (helper l k 1);;
(* ========================================================================================== *)



(* ========================================================================================== *)
exception List_Sorting_Error of string

let rec isDescending l = match l with                                                
| [] -> true                                                                     
 | [x] -> true                                                                    
 | x::y::ys -> if x >= y then isDescending (y::ys) else false;;            

let rec rev l = 
  match l with 
  | [] -> []
  | hd::tl -> (rev tl)@[hd]
                                        
let rec merge l1 l2 = match l1, l2 with                                              
 | l1, [] -> (rev l1)                                                                   
 | [], l2 -> (rev l2)                                                                   
 | x::xs, y::ys -> if x > y then (merge xs l2)@[x] else (merge l1 ys)@[y];;       
                                        
let merge_list(l1, l2) =                                                             
 if (isDescending l1) && (isDescending l2) then                                   
  merge l1 l2                                                                  
 else 
   raise (List_Sorting_Error "Input lists are not sorted!");;
(* ========================================================================================== *)



(* ========================================================================================== *)
let num_distinct l =
  let compare a b =
      match (a,b) with
      | ((val1, occur1),(val2, occur2)) -> if (occur1 > occur2) then ~-1
                                          else if (occur1 = occur2) then (val1 - val2)
                                          else 1
  in
  let rec helper l prev occurences =
      match l with
      | [] -> (prev,occurences)::[]
      | h::t -> if (h = prev) then (helper t h (occurences+1)) else
                                  (prev,occurences)::(helper t h 1)
  in
  match l with
  | [] -> []
  | h::t -> List.sort compare (helper t h 1);;
(* ========================================================================================== *)



(* ========================================================================================== *)
let rec zip l1 l2 = match l1, l2 with
  | [], [] -> []
  | x::xs, y::ys -> (x, y)::(zip xs ys);;

let isMatrix m = 
  match m with
  | [] -> false
  | x::xs -> 
          List.fold_left (&&) (true) (
              List.map (fun row -> (List.length x) = List.length row) xs)

let validAdd m1 m2 = 
  (List.length m1 = List.length m2) && (
      (List.length (List.hd m1)) = (List.length (List.hd m2)));;	

let rec matrix_add m1 m2 =
  if (isMatrix m1) && (isMatrix m2) then
      if (validAdd m1 m2) then
          let addRows(r1, r2) = 
              let addElem(e1, e2) = e1 + e2 in
              List.map (addElem) (zip r1 r2) in   
          List.map (addRows) (zip m1 m2)
      else
          raise (Non_matching_dimensions "Dimensions of the two matrices do not match")
  else
      raise (Not_a_Matrix "You have not given a valid matrix");;
(* ========================================================================================== *)



(* ========================================================================================== *)
let isMatrix m = 
  match m with
  | [] -> false
  | x::xs -> 
          List.fold_left (&&) (true) (
              List.map (fun row -> (List.length x) = List.length row) xs)

let rec mapn f lists =
  if List.mem [] lists then []
  else 
      f (List.map List.hd lists)::(mapn f (List.map List.tl lists))

let validMult m1 m2 = 
  (List.length (List.hd m1)) = List.length m2;;

let matrix_mult m1 m2 =
   if (isMatrix m1) && (isMatrix m2) then
      if (validMult m1 m2) then
          List.map (
              fun row -> mapn (
                  fun col -> List.fold_left (+) 0 (List.map2 ( * ) row col)) m2)  m1
      else
          raise (Non_matching_dimensions "Dimensions of the two matrices not suitable for multiplication")
  else
      raise (Not_a_Matrix "You have not given a valid matrix");;
(* ========================================================================================== *)