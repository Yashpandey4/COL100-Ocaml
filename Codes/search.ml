
let rec linelist input_chan lines = 
  try 
    let line = input_line input_chan in  
      if(String.length line > 0) then
        linelist input_chan (line :: lines)
      else   
        linelist input_chan (lines)
  with e ->   
    List.rev lines;;


let modify s = 
  let s1 = String.lowercase s in 
  let rec aux res n= 
    if(n<String.length s1) then
      let a = s1.[n] in
        if a<>',' && a<>':' && a<>'[' && a<>']' && a<>'(' && a<>')' then
          aux (res^(String.make 1 a)) (n+1)
        else aux (res^" ") (n+1) 
    else res in String.trim (aux "" 0);;

let splitchar s =
  let s1 = modify s in
  let rec aux str = 
    if(String.contains str ' ') then
      let a = String.index str ' ' in
      let x = String.sub str 0 a in
        if String.length x > 0 && x<>"," && x<>"." then (x)::aux((String.sub str (a+1) ((String.length str)-a-1)))
        else (aux(String.sub str (a+1) ((String.length str)-a-1)))
    else 
      let b = String.rindex s1 ' ' in
      let x = String.sub s1 (b+1) ((String.length s1)-b-1) in
        if String.length x > 0 && x<>"," && x<>"." then [x] else [] in
    aux ((modify s));;

let rec internalarray x = 
  let rec aux n =
    if(n<List.length x) then
      Array.of_list(List.nth x n)::aux (n+1)
    else [] in
    aux 0;;

let rec intextlist x = 
  let rec aux n =
    if(n<Array.length x) then
      Array.to_list(x.(n))::aux (n+1)
    else [] in
    aux 0;;

let finalarray l = 
  let rec aux1 n = 
    if(n<=(List.length l)-1) then
      splitchar (List.nth l n)::aux1 (n+1)
    else [] in
    Array.of_list(internalarray (aux1 0));;

let stringer arr = 
  let res = ref "" in
    for i=0 to ((Array.length arr)-1) do
      res:=!res^" "^string_of_int(arr.(i))
    done;
    (String.trim !res)^"\n";;

let encode list =
  let rec aux count acc = function
    | [] -> [] 
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
        else aux 0 ((count+1,a) :: acc) t in
    List.rev (aux 0 [] list);; 

let rec insert x l = 
  match l with
    |[]->[x]
    |h::t->
        if x<=h then x::h::t
        else h::(insert x t);;

let rec sort l =
  match l with
    |[]->[]
    |h::t->insert h (sort t);;

let rec insert1 x l = 
  let (x1,x2) = x in
    match l with
      |[]->[x]
      |h::t->
          let (h1,h2) = h in
            if x2<=h2 then x::h::t
            else h::(insert1 x t);;

let rec sort1 l =
  match l with
    |[]->[]
    |h::t->insert1 h (sort1 t);;

(*sort [1;2;3;34;45;6;6;6;6;5]*)

let is_contiguous arr = 
  let b=ref false in
  let final=ref true in
    for n=0 to ((Array.length arr)-2) do
      for i=0 to ((Array.length arr.(n+1))-1) do
        for j=0 to ((Array.length arr.(n))-1) do
          if arr.(n+1).(i)-arr.(n).(j) = 1 then
            b:=true
          else
            ()
        done
      done;
      final:=!final && !b;
      b:=false;
    done;
    !final;;

let posslist s =
  let rec aux str = 
    if(String.contains str ';') then
      let a = String.index str ';' in
      let x = String.sub str 0 a in
        if String.length x > 0 then (int_of_string x)::aux((String.sub str (a+1) ((String.length str)-a-1)))
        else (aux(String.sub str (a+1) ((String.length str)-a-1)))
    else 
    if(String.contains s ';') then
      let b = String.rindex s ';' in
      let x = String.sub s (b+1) ((String.length s)-b-1) in
        if String.length x > 0 then [(int_of_string x)] else []
    else
    if String.length s > 0 then [(int_of_string s)] else [] in
    Array.of_list(aux (s));;

let rec inprint1 l=
  let rec aux n = 
    if(n<(List.length l)) then
      let (a,b) = (List.nth l n) in
        (Printf.sprintf "%i %i\n" a b)^(aux (n+1))
    else 
      "" in
    print_string (aux 0);;

let rec inprint l=
  let rec aux n = 
    if(n<(List.length l)) then
      (string_of_int (List.nth l n))^"\n"^(aux (n+1))
    else 
      "" in
    print_string (aux 0);;

let boolean arr terms = 
  let docs = ref [] in
  let () = for i=0 to ((Array.length terms)-1) do
      for j=0 to ((Array.length arr)-1) do
        if terms.(i) = arr.(j).(0) then
          for k=0 to ((Array.length arr.(j))-1) do
            if(k mod 3 = 2) then
              docs:=int_of_string(arr.(j).(k))::!docs
            else
              ()
          done
        else 
          ()
      done
    done in
  let doc = Array.of_list (encode (sort !docs)) in
  let res = ref [] in
  let () = for i=0 to ((Array.length doc)-1) do
      let (freq,docid) = doc.(i) in
        if(freq = (Array.length terms)) then
          res:=docid::!res
        else 
          ()
    done in
    (Array.of_list(sort(!res)));;

let ranked arr terms maxres = 
  let docs = ref [] in
  let booleandocs = boolean arr terms in
  let () = for i=0 to ((Array.length terms)-1) do
      for j=0 to ((Array.length arr)-1) do
        if terms.(i) = arr.(j).(0) then
          for k=0 to ((Array.length arr.(j))-1) do
            if(k mod 3 = 2) then
              (
                docs:=(int_of_string(arr.(j).(k)),float_of_string(arr.(j).(1)),float_of_string(arr.(j).(k+1)))::!docs;
              )
            else
              ()
          done
        else 
          ()
      done
    done in
  let finaldocs = ref [] in
  let () = for i=((Array.length booleandocs)-1) downto 0 do
      for j=0 to ((List.length !docs)-1) do
        let (docid,idf,tf) = List.nth !docs j in
          if(docid = booleandocs.(i)) then
            finaldocs:=(docid,idf*.tf)::!finaldocs
          else ()
      done
    done in
  let res = ref 0. in
  let ranks = ref [] in
  let () = for i=((Array.length booleandocs)-1) downto 0 do
      for j=0 to ((List.length !finaldocs)-1) do
        let (docid,tfidf) = List.nth !finaldocs j in
          if(docid = booleandocs.(i)) then
            res:=!res+.tfidf
          else ()
      done;
      (
        ranks:=(booleandocs.(i),!res)::!ranks;
        res:=0.;
      )
    done in
  let () = ranks:=(List.rev(sort1 !ranks)) in
  let doclist = ref [] in
  let () = if(maxres<=List.length !ranks) then
      for i=0 to maxres-1 do
        let (docid,score) = List.nth !ranks i in
          doclist:=(docid)::!doclist
      done 
    else
      for i=0 to ((List.length !ranks)-1) do
        let (docid,score) = List.nth !ranks i in
          doclist:=(docid)::!doclist
      done in
    String.trim(stringer(Array.of_list (List.rev !doclist)));;

let phrase arr terms = 
  let docs = ref [] in
  let booleandocs = boolean arr terms in
  let allpos = ref [||] in
  let () = for i=0 to ((Array.length terms)-1) do
      for j=0 to ((Array.length arr)-1) do
        if terms.(i) = arr.(j).(0) then
          for k=0 to ((Array.length arr.(j))-1) do
            if(k mod 3 = 2) then
              (
                allpos:=Array.append !allpos (posslist (arr.(j).(k+2)));
                docs:=(int_of_string(arr.(j).(k)),!allpos)::!docs;
                allpos:=[||];
              )
            else
              ()
          done
        else 
          ()
      done
    done in 
  let finaldocs = ref [] in
  let () = for i=((Array.length booleandocs)-1) downto 0 do
      for j=0 to ((List.length !docs)-1) do
        let (docid,pos) = List.nth !docs j in
          if(docid = booleandocs.(i)) then
            finaldocs:=(docid,pos)::!finaldocs
          else ()
      done
    done in
  let res = ref [||] in
  let phraser = ref [] in
  let () = for i=0 to ((Array.length booleandocs)-1) do
      for j=0 to ((List.length !finaldocs)-1) do
        let (docid,pos) = List.nth !finaldocs j in
          if(docid = booleandocs.(i)) then
            res:=Array.append !res [|pos|]
          else ()
      done;
      (
        if(is_contiguous (!res))then
          (
            phraser:=(booleandocs.(i))::!phraser;
            res:=[||];
          )
        else
          ()
      )
    done in
    String.trim(stringer(Array.of_list (sort !phraser)));;

let aux l terms switch maxres = 
  let arr = finalarray l in
  let res = ref "" in
  let () =
    match switch with
      |"-b" -> res:=String.trim(stringer (boolean arr terms))
      |"-r" -> res:=ranked arr terms maxres
      |"-p" -> res:=phrase arr terms
      |_->() in
    !res;;

(*finalarray ["the 0.17609 [(0:0.25:[0;6]),(1:0.07692:[4])]"]*)

let () =

  (*let l = read_filedoc(Sys.argv.(1)) in*)
  let switch = (Sys.argv.(1)) in
  let k=ref 2 in
  let maxres = ref (-1) in
  let () =
    (
      maxres:=int_of_string(Sys.argv.(!k));
      incr k;
    )
  in
  let numterm = int_of_string(Sys.argv.(!k)) in
  let terms = Array.make numterm "" in
  let () = 
    incr k;
    for i=0 to (numterm-1) do
      terms.(i)<-Sys.argv.(!k);
      incr k;
    done in
  let l = (linelist (open_in (Sys.argv.(!k))) []) in
  let s = ref "" in
  let () = s:= String.trim (aux l terms switch !maxres) in
    print_string ((String.trim !s));;
(*with e-> print_string "main throws an error\n";;*)

