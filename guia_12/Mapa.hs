data Dir = Left' | Right' | Straight' deriving Show
data Mapa a = Cofre [a]
            | Nada (Mapa a)
            | Bifurcacion [a] (Mapa a) (Mapa a) deriving Show

subst f g x = f x (g x)

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc _ _ (Cofre xs) = fc xs
foldM fc fn fb (Nada m) = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion xs m1 m2) = fb xs (foldM fc fn fb m1) (foldM fc fn fb m2)

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> Mapa a -> b -> b -> b) -> Mapa a -> b
recM fc _ _ (Cofre xs) = fc xs
recM fc fn fb (Nada m) = fn m (recM fc fn fb m)
recM fc fn fb (Bifurcacion xs m1 m2) = fb xs m1 m2 (recM fc fn fb m1) (recM fc fn fb m2)

objects :: Mapa a -> [a]
objects = foldM id id ((.)(.)(.) (++) (++))

mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f = foldM (Cofre . map f) id (Bifurcacion . map f)

has :: (a -> Bool) -> Mapa a -> Bool
has p = foldM (any p) id (\xs b1 b2 -> any p xs || b1 || b2)

hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt p = foldM explorarCofre explorarNada explorarBifuracion
              where explorarCofre xs [] = any p xs                     -- Reviso el cofre 
                    explorarCofre _ _ = False                          -- Si llego a un cofre y quedan direcciones, no es por ahi 

                    explorarNada h (Straight':ds) = h ds               -- Avanzo
                    explorarNada _ _ = False                           -- Me quede sin pasos o es un paso invalido

                    explorarBifuracion xs _ _ [] = any p xs            -- Reviso los objetos de la bifurcacion
                    explorarBifuracion _ h1 _ (Left':ds) = h1 ds       -- Todavia quedan direcciones
                    explorarBifuracion _ _ h2 (Right':ds) = h2 ds      -- Todavia quedan direcciones
                    explorarBifuracion _ _ _ (Straight':_) = False     -- Me piden avanzar en una bifurcacion

longestPath :: Mapa a -> [Dir]
longestPath = foldM (const []) (Straight':) (\_ ds1 ds2 -> if length ds1 > length ds2 then Left' : ds1 else Right' : ds2)

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath = subst objectsAt longestPath

objectsAt :: Mapa a -> [Dir] -> [a]
objectsAt = foldM explorarCofre explorarNada explorarBifuracion
              where explorarCofre xs _ = xs                        -- Siempre reviso el cofre 

                    explorarNada h (Straight':ds) = h ds            -- Avanzo
                    explorarNada _ _ = []                           -- Me quede sin pasos o es un paso invalido

                    explorarBifuracion xs h1 _ (Left':ds)  = xs ++ h1 ds   -- Todavia quedan direcciones
                    explorarBifuracion xs _ h2 (Right':ds) = xs ++ h2 ds   -- Todavia quedan direcciones
                    explorarBifuracion xs _ _ _           = xs             -- No quedan direcciones o son invalidas

            -- Suponiendo que son los objetos al final nomas
            --   where explorarCofre xs [] = xs                        -- Reviso el cofre 
            --         explorarCofre _ _ = []                          -- Si llego a un cofre y quedan direcciones, no es por ahi 

            --         explorarNada h (Straight':ds) = h ds            -- Avanzo
            --         explorarNada _ _ = []                           -- Me quede sin pasos o es un paso invalido

            --         explorarBifuracion xs _ _ [] = xs               -- Reviso los objetos de la bifurcacion
            --         explorarBifuracion _ h1 _ (Left':ds) = h1 ds    -- Todavia quedan direcciones
            --         explorarBifuracion _ _ h2 (Right':ds) = h2 ds   -- Todavia quedan direcciones
            --         explorarBifuracion _ _ _ (Straight':_) = []     -- Me piden avanzar en una bifurcacion

allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (const []) 
                 (\dss -> [Straight'] : map (Straight':) dss) 
                 (\_ dss1 dss2 -> [Left'] : [Right'] : map (Left':) dss1 ++ map (Right':) dss2)

m = Bifurcacion [1,2] (Nada (Cofre [3,4])) (Bifurcacion [5,6] (Cofre [7,8]) (Nada (Cofre [9,10])))
m2 = Bifurcacion [1,2] (Nada (Cofre [3,4])) (Bifurcacion [5,6] (Cofre [7,8]) (Bifurcacion [9,10] (Cofre [11,12]) (Nada (Cofre [13,14]))))

objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel = foldM (:[]) ([]:) (\xs xss yss -> xs : merge xss yss)

merge :: [[a]] -> [[a]] -> [[a]]
merge = foldr g id
      where g xs h [] = xs : h []
            g xs h (ys:yss) = (xs ++ ys) : h yss

