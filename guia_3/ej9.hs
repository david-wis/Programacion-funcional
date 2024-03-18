appDup f x = f (x, x)

many 0 f x = x
many n f x = f ( many (n-1) f x )

appFork (f,g) x = (f x, g x)

compose f g x = f ( g x )

subst f g x = f x (g x)



cuadruple = many 2 (appDup (uncurry (+)))

timesTwoPlusThree = uncurry (+) . appFork (id, succ . succ . succ)

fourTimes f = many 4 (flip subst f (flip const))
