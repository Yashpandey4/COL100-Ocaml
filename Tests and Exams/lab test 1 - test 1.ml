

      let valid n a b c d = (a>0 && b>0 && c>0 && d>0 && n>0 && a<>b && a<>c && a<>d && b<>c && b<>d && c<>d);;

      let rec aux n a b c d = 
        if(n>a || n=a) then
        aux (n-a) a b c d + aux n (n+1) b c d
        else if (n>b || n=b) then
        aux (n-b) (n+1) b c d + aux n (n+1) (n+1) c d
        else if (n>c || n=c) then
        aux (n-c) (n+1) (n+1) c d + aux n (n+1) (n+1) (n+1) d
        else if (n>d || n=d) then
        aux (n-d) (n+1) (n+1) (n+1) d + aux n (n+1) (n+1) (n+1) (n+1)
        else if(n=0) then 1
        else 0;;
        
        let rec coinChanger n a b c d =
          if(valid n a b c d) then 
          aux n a b c d
          else
          (-1);;
