module Array = struct
  include Array

  let foldmap g f a =
    let n = Array.length a in
    let rec aux acc i =
      if i >= n then acc else aux (g acc (f a.(i))) (succ i)
    in aux (f a.(0)) 1


  let foldi_left f x a =
    let r = ref x in
      for i = 0 to length a - 1 do
        r := f i !r (unsafe_get a i)
      done;
      !r
end

let foldmap_range g f (a,b) =
  let rec aux acc n =
    let n = succ n in
      if n > b then acc else aux (g acc (f n)) n
  in aux (f a) a

let fold_range f init (a,b) =
  let rec aux acc n =
    if n > b then acc else aux (f acc n) (succ n)
  in aux init a


let swap_elem m i j = let x = m.(i) in m.(i) <- m.(j); m.(j) <- x
let maxtup a b = if (snd a) > (snd b) then a else b
let augmented_matrix m b =
  Array.(init (length m) ( fun i -> append m.(i) [|b.(i)|] ))


let solve m b =
  let n = Array.length m in
  let n' = pred n in (* last index = n-1 *)
  let s = Array.(map (foldmap max abs_float) m) in  (* scaling vector *)
  let a = augmented_matrix m b in

    for k = 0 to pred n' do
      let pair i = (i, abs_float a.(i).(k) /. s.(i)) in
      let i_max,v = foldmap_range maxtup pair (k,n') in
        if v < epsilon_float then failwith "Matrix is singular.";
        swap_elem a k i_max;
        swap_elem s k i_max;

        for i = succ k to n' do
          let tmp = a.(i).(k) /. a.(k).(k) in
            for j = succ k to n do
              a.(i).(j) <- a.(i).(j) -. tmp *. a.(k).(j);
            done
        done
    done;


    let x = Array.copy b in 
      for i = n' downto 0 do
        let minus_dprod t j = t -. x.(j) *. a.(i).(j) in
          x.(i) <- fold_range minus_dprod a.(i).(n) (i+1,n') /. a.(i).(i);
      done;
      x

let rec internalarray x = 
  let rec aux n =
    if(n<List.length x) then
      Array.of_list(List.nth x n)::aux (n+1)
    else [] in
    aux 0;;

let rec internallist x = 
  let rec aux n =
    if(n<Array.length x) then
      Array.to_list(x.(n))::aux (n+1)
    else [] in
    aux 0;;

let rec transpose m =
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transpose (List.map List.tl m);;

let solveRowEchelon x = 
  let hd::tl = List.rev (transpose x) in
  let b = hd in
  let m = transpose (List.rev tl) in
  let res = solve (Array.of_list (internalarray m)) (Array.of_list b) in
    Array.to_list res;;

