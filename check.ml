type cell =
| Value of int
| PossibleValues of int list

let int_of_value v = 
match v with 
|Value (x) -> x
|_->(-1);;

let list_of_pvalues v = 
match v with 
|PossibleValues (x) -> x
|_ -> [];;

let pvalues_of_list v = 
match v with 
|x->PossibleValues (x);;


let checkifvalue v = 
let b = ref true in
(if int_of_value (v) = (-1) then b:=false else ());
!b;;

let remove l r = 
let arr = Array.of_list l in
let index = ref (-1) in
for i=0 to (Array.length arr)-1 do
if arr.(i) = r then
index:=i
else ()
done;
if (!index =(-1)) then l
else let newarr = Array.append (Array.sub arr 0 (!index)) (Array.sub arr (!index+1) ((Array.length arr)-(!index+1))) in
Array.to_list newarr;;

let eliminateValueRow sudoku v r = 
let b = ref false in
for j=0 to 8 do 
if checkifvalue sudoku.(r).(j) = false then 
(sudoku.(r).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(r).(j)) v);
b:=true)
else ()
done;
!b

let eliminateValueCol sudoku v c = 
let b = ref false in
for i=0 to 8 do 
if checkifvalue sudoku.(i).(c) = false then 
(sudoku.(i).(c)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(c)) v);
b:=true)
else ()
done;
!b 

let eliminateValueBox sudoku v b = 
let flag = ref false in
(
if(b=0) then
for i=0 to 2 do 
for j=0 to 2 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v);
flag:=true)
else ()
done
 done


else if(b=1) then
for i=0 to 2 do 
for j=3 to 5 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v);
flag:=true)
else ()
done
 done

else if(b=2) then
for i=0 to 2 do 
for j=6 to 8 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v);
flag:=true)
else ()
done
 done

else if(b=3) then
for i=3 to 5 do 
for j=0 to 2 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v); flag:=true) else ()
done
 done

else if(b=4) then
for i=3 to 5 do 
for j=3 to 5 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v); flag:=true) else ()
done
 done

else if(b=5) then
for i=3 to 5 do 
for j=6 to 8 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v); flag:=true) else ()
done
 done

else if(b=6) then
for i=6 to 8 do 
for j=0 to 2 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v); flag:=true) else ()
done
 done

else if(b=7) then
for i=6 to 8 do 
for j=3 to 5 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v); flag:=true) else ()
done
 done

else
for i=6 to 8 do 
for j=6 to 8 do
if checkifvalue sudoku.(i).(j) = false then 
(sudoku.(i).(j)<-pvalues_of_list (remove (list_of_pvalues sudoku.(i).(j)) v); flag:=true) else ()
done
 done
);
!flag;;

let eliminate sudoku i j = 
let b = ref false in
(if int_of_value (sudoku.(i).(j)) <> (-1) then 
(eliminateValueRow sudoku (int_of_value sudoku.(i).(j)) i;
eliminateValueCol sudoku (int_of_value sudoku.(i).(j)) j;
b:=true;)
else ());
!b
