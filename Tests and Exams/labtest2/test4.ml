
let rec transpose m =
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transpose (List.map List.tl m);;

let rec numSolutions x = 
  let rec allzero r n =
    if(n<(List.length r)-1) then
      if(List.nth r n <> 0.) then false
      else allzero r (n+1)
    else if(List.nth r n <> 0.) then true else false in
  let rec nosoln x n = 
    if(n<(List.length x)) then
      (allzero (List.nth x n) 0) ||  nosoln x (n+1)
    else false in

  let rec allzero1 r n =
    if(n<(List.length r)) then
      if(List.nth r n <> 0.) then false
      else allzero r (n+1)
    else true in
  let rec onesoln r n = (*false if one solution exists*)
    if(n<(List.length x)-1) then
      (allzero1 (List.nth x n) 0) ||  nosoln x (n+1)
    else false in

    if (nosoln x 0) then 0
    else if ((nosoln x 0)=false && (onesoln (transpose x) 0) = false) then 1
    else max_int;;

numSolutions [[1.0;0.0;1.0;4.0];[0.0;1.0;-2.0;0.0];[0.0;0.0;0.0;0.0]]
