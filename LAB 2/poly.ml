(* Function check_poly takes three integers and returns true or false *)
let check_poly x y z= (x>=0 && y>=0 && z>=0) 
(* then
   true
else
false *)






(* Function poly takes three integers and returns an integer *)
let poly x y z= 
if(check_poly x y z) then 
   (int_of_float(float_of_int(x)**3.0))+(2*x*y*int_of_float(float_of_int(z)**2.0))-(y*z)+1
else
-1


(*let () =print_int (poly 3 5 6);
        print_string "\n"*)

