type matrix = int list list

  let rec zip lst1 lst2 = match lst1,lst2 with
    | [],_ -> []
    | _, []-> []
    | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)

  let rec unzip l =
    match l with
      [] -> [],[]
    | (x,y)::t ->
        let xs,ys = unzip t in x::xs,y::ys

 let rec tabulate n f = let rec aux x = (if x == n then [] else (f x)::(aux (x+1))) in aux 0

  let rec range (i: int) (j: int) = if i > j then [] else i :: range (i+1) j

  let even (x: int) = x mod 2 = 0

  let sqMatrix (size: int) (value: int) : matrix =
    tabulate size (fun x -> tabulate size (fun y -> value))

  let getMinor (m: matrix) ((i: int), (j: int)) : matrix =
    let without xs n =
      snd (unzip
        (List.filter (fun (i, e) -> i <> n)
                    (zip (range 1 (List.length xs)) xs)) )
    in
      List.map (fun r -> without r j) (without m i)

  let getFirstRowPairs(m : matrix) : (int * int) list =
    zip (tabulate (List.length m) (fun x -> 1)) (range 1 (List.length m))

  let getCell (m: matrix) ((i: int), (j: int)) : int =
    List.nth (List.nth m (i - 1)) (j - 1)

  let evensNegative(xs : int list) : int list =
    List.map (fun (f, s) -> if f then -s else s)
    (zip (List.map even (range 1 (List.length xs))) xs)

  let rec getDeterminant(m: matrix) : int =
    if List.length m == 1
    then getCell m (1, 1)
    else
      List.fold_right (fun x y -> x + y)
      (evensNegative(List.map (fun p -> (getCell m p) * (getDeterminant (getMinor m p)))
                        (getFirstRowPairs m))) 0


let m = [[1;2;3];[4;5;6];[7;8;9]];;

 getDeterminant m;;
    


