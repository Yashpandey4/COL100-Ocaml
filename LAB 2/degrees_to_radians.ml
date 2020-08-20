(* Function check_degrees_to_radians takes one float and returns a float *)
let check_degrees_to_radians x = if (x>=0.0) then
mod_float x 360.0

else 
mod_float (360.0+.(mod_float x 360.0)) 360.0;;

(* Function degrees_to_radians takes one float and returns a float *)
let degrees_to_radians d = let pi=4.0*.atan(1.0) in (check_degrees_to_radians d)*.pi/.180.0

