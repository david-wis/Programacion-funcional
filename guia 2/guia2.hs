twice f = g 
  where g x = f ( f x)

swap (x,y) = (y,x)

uflip f = g
  where g p = f ( swap p )

subst f = h
  where h g = k 
          where k x = (f x) (g x)

flip f = h 
  where h x = k
         where k y = (f y) x


apply f = g
  where g x = f x
