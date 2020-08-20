let rec hanoi n =
let rec printhanoi n a b c =
if n <> 0 then 
(printhanoi (pred n) a c b)^(Printf.sprintf "(%d, %d)\n" a b)^(printhanoi (pred n) c b a)
else
"" in
printhanoi n 1 3 2;;

