let fizzbuzz x =
if(x mod 15 = 0) then
"Fizzbuzz"
else if(x mod 3 = 0) then
"Fizz"
else if(x mod 5 = 0) then
"Buzz"
else
string_of_int x;;
let rec fizzbuzz_string y = 
let s=fizzbuzz y in
if(s<>"1") then
fizzbuzz_string (y-1)^" "^s
else
"1";;
