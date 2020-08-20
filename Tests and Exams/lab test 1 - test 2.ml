
let valid n a b c d = (a>0 && b>0 && c>0 && d>0 && n>0 && a<>b && a<>c && a<>d && b<>c && b<>d && c<>d);;



let laz n a f =
  if (n>=0) then (n + (f a))
  else
    (-1);;

let mini a b =
  if ((a>b && b>=0) || a<0) then b   
  else a;;


let rec cost_check n a b c d f=
  if (n>=a) then
    let g = (cost_check (n-a) a b c d f) in
    let e = laz g a f in
    let h = (cost_check (n) (n+1) (b) (c) (d) (f)) in
      (mini e h)

  
  else if (n>=b) then
    let g = (cost_check (n-b) (n+1) b c d f) in
    let e = laz g b f in
    let h = (cost_check (n) (n+1) (n+1) (c) (d) (f)) in
      (mini e h)

  
  else if (n>=c) then
    let g = (cost_check (n-c) (n+1) (n+1) c d f) in
    let e = laz g c f in
    let h = (cost_check (n) (n+1) (n+1) (n+1) (d) (f)) in
      (mini e h)
  
  else if (n>=d) then
    let g =(cost_check (n-d) (n+1) (n+1) (n+1) d f) in
    let e = laz g d f in
    let h = (cost_check (n) (n+1) (n+1) (n+1) (n+1) (f)) in
      (mini e h)

  
  else if (n=0) then (0)
  else  (-1);;

let rec coinChanger_cost n a b c d f =
  if(valid n a b c d) then 
    let g =(cost_check n a b c d f) in
      if(g>=0) then g
      else 
        max_int
  else
    (-1);;

coinChanger_cost 10 3 4 5 6 (fun x->(int_of_float(2.0**float_of_int(x))));;

