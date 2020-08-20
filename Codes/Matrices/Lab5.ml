exception Failure of string;;
exception Invalid_argument of string;;

module MyList = struct

let rev list =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h::acc) t in
    aux [] list;;

let length list =
    let rec aux n = function
      | [] -> n
      | _::t -> aux (n+1) t
    in aux 0 list;;

  let rec nth l n =
if(n<0) then raise (Invalid_argument "MyList.nth")
else
    match l with
      | [] ->  raise (Failure "nth")
      | hd::tl -> if(n>0) then (nth tl (n-1))
          else hd


  let rec flatten l =
    match l with
      |[] -> []
      |hd::tl -> hd@(flatten tl)

  let rec map f l = 
    match l with
      |[]->[]
      |hd::tl-> (f hd)::(map f tl)

  let rec rev_map f l = 
    let rec aux f l = 
      match l with
        |[]->[]
        |hd::tl-> (f hd)::(aux f tl) in
      rev (aux f l)

 let rec fold_left f r l = 
    let rec aux n = 
      if(n>=0) then
	f (aux (n-1)) (nth l n)
      else
	r in
      aux ((length l)-1)

let rec fold_right f l s =
    let rec aux n =
      if(n<=(length l)-1) then
	f (nth l n) (aux (n+1))
      else
        s in
      aux (0)

 let rec map2 f l1 l2 = 
if((length l1)<>(length l2)) then raise (Invalid_argument "")
else
    let rec aux n = 
      if(n>=0) then
        (f (nth l1 n) (nth l2 n))::(aux (n-1))
      else 
        [] in
      rev (aux ((length l1)-1))

 let rec fold_left2 f r l1 l2 = 
if((length l1)<>(length l2)) then raise (Invalid_argument "")
else
    let rec aux n =
      if(n>=0) then
        (f (aux (n-1)) (nth l1 n) (nth l2 n))
      else
        r in
      aux ((length l1)-1)

  let rec for_all f l =
    match l with
      |[]-> true
      |hd::tl -> (f hd)&&(for_all (f) (tl))

  let rec exists f l =
    match l with
      |[]->false
      |hd::tl -> (f hd)||(exists (f) (tl))


  let rec filter f l =
    match l with
      |[]->[]
      |hd::tl -> if(f hd) then hd::(filter (f) (tl))
          else (filter (f) (tl))

 
 
  
 

end

 