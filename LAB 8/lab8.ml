open Printf

let pi = 4.*.atan(1.);;
let rec linelist () = 
let ic = stdin in
  try 
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    line :: (linelist ());              (* write on the underlying device now *)
                    (* close the input channel *) 
  
  with e ->   
      [];;
(*
let linelist () = 
  let rec read_recursive lines =
    try
      Scanf.fscanf stdin "%[^\r\n]\n" (fun x -> read_recursive (x :: lines))
    with
        End_of_file ->
          lines in
  let lines = read_recursive [] in
  let _ = close_in_noerr stdin in
    List.rev (lines);;
*)

let splitchar s = 
  let rec aux str = 
    if(String.contains str ' ') then
      let a = String.index str ' ' in
        (String.sub str 0 a)::aux ((String.sub str (a+1) ((String.length str)-a-1)))
    else 
      let b = String.rindex s ' ' in
        [String.sub s (b+1) ((String.length s)-b-1)] in
    aux (s);;

let strev s = 
  let rec revs strin list index =
    if List.length list = String.length strin
    then String.concat "" list
    else revs strin ((String.sub strin index 1)::list) (index+1) in
    revs s [] 0;;


module Geometry = struct

  let rec listcreator llist = 
    let rec aux2 n shapelines outlines=
      if(n<(List.length llist)) then 
        let line = List.nth llist n in
        let l = splitchar (String.trim line) in
          if((List.nth l 0).[(String.length (List.nth l 0))-1] = ':') then
            aux2 (n+1) (line::shapelines) outlines
          else
            aux2 (n+1) (shapelines) (line::outlines)
      else
        (List.rev shapelines, List.rev outlines) in
      aux2 0 [] [];;


  let p1 l = 
    let rec aux x y n = 
      if(n>=1) then
        let s = List.nth l n in
        let n1 = String.index s '(' in
        let n2 = String.index s ',' in
        let n3 = String.index s ')' in
          aux (int_of_string(String.sub s (n1+1) (n2-n1-1))::x) (int_of_string(String.sub s (n2+1) (n3-n2-1))::y) (n-1)
      else 
        (x,y) in 
      aux [] [] ((List.length l)-1);;

  let t1 x y = 
    match (List.length x) with
      |1->"P"
      |2->"L"
      |x-> "PLG "^string_of_int(x);;

  let t s = 
    if(s.[(String.length s)-1]<>')') then
      "C"
    else
      let wl = splitchar (String.trim s) in
      let (x,y) = p1 wl in
        t1 x y;;

  let p2 x y = 
    let n = List.length x in
    let rec aux i j res = 
      if (i<n) then
        let a = sqrt ((float_of_int((List.nth x i)-(List.nth x j))**(2.))+.(float_of_int((List.nth y i)-(List.nth y j))**(2.))) in
          aux (i+1) (i) (res+.a)
      else 
        res in
      aux 0 (n-1) 0.;;



  let p s = 
    if(s.[(String.length s)-1]<>')') then
      let b = String.rindex s ' ' in
      let n = int_of_string(String.sub s (b+1) ((String.length s)-b-1)) in
        string_of_float (2.*.pi*.float_of_int(n))
    else
      let wl = splitchar (String.trim s) in
      let (x,y) = p1 wl in
        if (t s = "P") then string_of_float (0.)
        else if (t s = "L") then string_of_float ((p2 x y)/.2.)
        else string_of_float (p2 x y);;

  let a1 x y = 
    let n = List.length x in
    let rec aux i j res = 
      if(i<n) then
        let a = ((List.nth x j)+(List.nth x i))*((List.nth y j)-(List.nth y i)) in
          aux (i+1) (i) (res+a)
      else
        abs_float ((float_of_int (res))/.(2.)) in
      aux 0 (n-1) 0;;

  let a s = 
    if(s.[(String.length s)-1]<>')') then
      let b = String.rindex s ' ' in
      let n = int_of_string(String.sub s (b+1) ((String.length s)-b-1)) in
        string_of_float (pi*.((float_of_int(n))**(2.)))
    else
      let wl = splitchar (String.trim s) in
      let (x,y) = p1 wl in
        string_of_float (a1 x y);;



  (*centroid calc begins*)
  let polycent x y = 
    let n = List.length x in
    let rec aux i resx resy = 
      if(i<n) then
        aux (i+1) (resx+(List.nth x i)) (resy+(List.nth y i))
      else
        (float_of_int(resx)/.float_of_int(n),float_of_int(resy)/.float_of_int(n)) in
      aux 0 0 0;;

  let circlecent wl = 
    let s = (List.nth wl 1) in
    let n1 = String.index s '(' in
    let n2 = String.index s ',' in
    let n3 = String.index s ')' in
      (float_of_string(String.sub s (n1+1) (n2-n1-1))),(float_of_string(String.sub s (n2+1) (n3-n2-1)))
  ;;

  let d1 s = 
    if(t s = "C") then
      let wl = splitchar (String.trim s) in
        circlecent wl

    else 
      let wl = splitchar (String.trim s) in
      let (x,y) = p1 wl in
        polycent x y;;



  let d s1 s2 = 
    let (a1,b1) = d1 s1 in
    let (a2,b2) = d1 s2 in
    let res = sqrt (((a2-.a1)**(2.))+.((b2-.b1)**(2.))) in
      string_of_float res;;


  let decide s rsl = 
    match s.[0] with
      |'P'-> 
          let b = String.rindex s ' ' in
          let n = int_of_string(String.sub s (b+1) ((String.length s)-b-1)) in
            p (List.nth rsl (n-1))
      |'A'->
          let b = String.rindex s ' ' in
          let n = int_of_string(String.sub s (b+1) ((String.length s)-b-1)) in
            a (List.nth rsl (n-1))
      |'T'->
          let b = String.rindex s ' ' in
          let n = int_of_string(String.sub s (b+1) ((String.length s)-b-1)) in
            t (List.nth rsl (n-1))
      |'D'->
          let b = String.rindex s ' ' in
          let a = String.index s ' ' in
          let n1 = int_of_string(String.sub s (a+1) (b-a-1)) in
          let n2 = int_of_string(String.sub s (b+1) ((String.length s)-b-1)) in
            d (List.nth rsl (n1-1)) (List.nth rsl (n2-1));;

  let rec aux1 llist =
    let (sl,ol)=listcreator llist in (*shapelist, outlist*)
    let rec aux2 n = 
      if(n>=0) then  
        let s = String.trim (decide (List.nth ol n) sl) in
        let u = aux2 (n-1) in 
          if(u = "") then (u)^s 
          else (u)^"\n"^s
      else
        "" in
      aux2 ((List.length ol)-1);;
end


let () = 
  let l = linelist () in 
  let s = String.trim (Geometry.aux1 l) in
  print_string s;;
