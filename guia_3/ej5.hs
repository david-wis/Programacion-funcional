appDup = \f -> \x -> f (x,x)

appFork = \(f,g) -> \x -> (f x, g x)

appPar = \(f,g) -> \(x,y) -> (f x, g x)

appDist = \f -> \(x,y) -> (f x, f y)

subst = \f -> \g -> \x -> f x (g x)
