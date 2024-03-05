id x = x

suma x = g
  where g y = x+y

const x = g
 where g y = x

compose f = h
  where h g = k
    where k x = f (g x)


subst f = h
  where h g = k
    where k x = f x (g x)



-- Mostrar la reduccion de 
-- 1)
-- a) ( suma 2 ) 3
-- b) ((subst const) suma) 17

-- 2) (subst const) suma