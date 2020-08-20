(* Function check_nlog takes one float and returns true or false *)
let check_nlog x = x+.(((x**2.0)+.1.0)**(1.0/.2.0))>0.0






(* Function nlog takes one float and returns a float *)
let nlog x = if (check_nlog x) then 
log (x+.(((x**2.0)+.1.0)**(1.0/.2.0)))
else
-1.0