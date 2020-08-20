open Printf

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

let splitchar s = 
let rec aux str = 
if(String.contains str ' ') then
let a = String.index str ' ' in
(String.sub str 0 a)::aux ((String.sub str (a+1) ((String.length str)-a-1)))
else 
let b = String.rindex s ' ' in
[String.sub s (b+1) ((String.length s)-b-1)] in
aux (s);;

let toIntArray s = 
let rec aux n =
if (n<>String.length s)then
int_of_string(String.make 1 s.[n])::aux (n+1)
else [] in
aux 0;;

let rmzero s =
let l = toIntArray s in
let rec aux list = 
match list with
|[]->[0]
|hd::tl -> if(hd = 0) then aux tl
else list in
String.concat "" (List.map string_of_int (aux l));;

let strev s = 
let rec revs strin list index =
if List.length list = String.length strin
then String.concat "" list
else revs strin ((String.sub strin index 1)::list) (index+1) in
revs s [] 0;;

module BigNumber = struct
type bignumber = BigNum of int list;;
end

module BigInteger = struct
(*adding begins*)
let rec adder1 s1 s2 = (*send the strings reversed,make sure length
of str2 is larger*)
let rec aux n carry =
if(n<String.length s1) then
let a = (int_of_string(String.make 1 s1.[n])+int_of_string(String.make 1 s2.[n]))+carry in
(a mod 10)::aux (n+1) (a/10)
else
[] in
aux 0 0;;


let rec adder2 s1 s2 = 
let oldcarry = (int_of_string(String.make 1 s1.[(String.length s1)-1])+int_of_string(String.make 1 s2.[(String.length s1)-1])) in
let half = adder1 s1 s2 in
let rec aux n1 n2 carry = 
if(n1 < n2) then 
let a = int_of_string(String.make 1 s2.[n1])+carry in
(a mod 10)::aux (n1+1) n2 (a/10)
else
[carry] in
List.rev (half @ (aux (String.length s1) (String.length s2) (oldcarry/10)));;


let add l = 
let rec aux n = 
if(n<List.length l) then
let b = aux (n+1) in
if(String.length (List.nth l n) > String.length b) then
rmzero (String.concat "" (List.map string_of_int (adder2 (strev b) (strev (List.nth l n)))))
else
rmzero (String.concat "" (List.map string_of_int (adder2 (strev (List.nth l n)) (strev b))))
else
"0" in
aux 0;;
(*adding ends*)

(*multiplying begins*)
let multiplier1 s n = 
let rec aux n = 
if(n>0) then
s::aux (n-1) 
else [] in
add (aux n);;

let multiplier2 s1 s2 = 
let rec aux n = 
if(n<String.length s2) then
let s = multiplier1 s1 (int_of_string (String.make 1 s2.[n])) in
(s^(String.make ((String.length s2)-n-1) '0'))::aux (n+1)
else
[] in 
add (aux 0);;

let mult l = 
let rec aux n = 
if(n<List.length l) then
let b = aux (n+1) in
multiplier2 b (List.nth l n)
else
"1" in
aux 0;;
(*multiplying ends*)

(*subtracting begins*)
let subtracter1 s1 s2 = (*s1>s2, both reversed*)
let rec aux n carry =
if(n<String.length s2) then
let a = (int_of_string(String.make 1 s1.[n])-int_of_string(String.make 1 s2.[n]))-carry in
if(a<0) then 
(a+10)::aux (n+1) (1)
else 
a::aux (n+1) 0
else
[] in
aux 0 0;;

let rec subtracter2 s1 s2 = 
let oldcarry = if((int_of_string(String.make 1 s1.[(String.length s2)-1])-int_of_string(String.make 1 s2.[(String.length s2)-1]))<0) then 1 else 0 in
let half = subtracter1 s1 s2 in
let rec aux n2 n1 carry = 
if(n2 < n1) then 
let a = int_of_string(String.make 1 s1.[n2])-carry in
if(a<0) then
(a+10)::aux (n2+1) n1 1
else
a::aux (n2+1) n1 0
else
[carry] in
List.rev (half @ (aux (String.length s2) (String.length s1) (oldcarry)));;

let sub l = 
rmzero (String.concat "" (List.map string_of_int (subtracter2 (strev (List.nth l 0)) (strev (List.nth l 1)))));;
(*subtracting ends*)

(*division begins*)
let divider1 s div = 
let l = toIntArray s in
let rec aux temp n =
if(temp<div) then
aux (temp*10 + (List.nth l n)) (n+1)
else (temp,n) in
aux 0 0;;

let divider2 s div = 
let l = toIntArray s in
let (otemp,on) = divider1 s div in 
let rec aux temp n = 
if(List.length l > n) then
(temp/div)::aux (((temp mod div) * 10) + List.nth l n) (n+1)
else
[(temp/div)] in
aux otemp (on);;

let div l = 
let a = List.nth l 0 in
let b = List.nth l 1 in
if(b="0") then "-1" else
if(String.length b > String.length a) then "0"
else if(String.length b > String.length (string_of_int max_int)) then "1"
else
rmzero (String.concat "" (List.map string_of_int (divider2 a (int_of_string b))));;

(*division ends*)

let decide line =
let l = splitchar (String.trim line) in
match l with
|"ADD"::tl -> add tl
|"MULT"::tl -> mult tl
|"SUB"::tl -> sub tl
|"DIV"::tl -> div tl;;


let rec aux1 llist =
let rec aux2 l n = 
if(n>=1) then  
let s = String.trim (decide (List.nth l n)) in
if(s = "-1") then (aux2 l (n-1))^"\n"^"NAN" 
else if(aux2 l (n-1) = "" && s = "-1") then (aux2 l (n-1))^"NAN" 
else if(aux2 l (n-1) = "") then (aux2 l (n-1))^s 
else (aux2 l (n-1))^"\n"^s
else
"" in
aux2 llist ((List.length llist)-1)

end

let () = 
let l = linelist () in 
let s = (BigInteger.aux1 l) in
let output_channel = open_out (List.nth l 0) in
fprintf output_channel "%s" (String.trim s);
close_out output_channel;;


