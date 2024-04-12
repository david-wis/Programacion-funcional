data Dungeon a = Habitacion a
                | Pasaje (Maybe a) (Dungeon a)
                | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

-- a)
cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion _) = 0
cantidadDeBifurcaciones (Pasaje _ d) = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion _ d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2


-- b)
cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion _) = 1
cantidadDePuntosInteresantes (Pasaje _ d) = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion _ d1 d2) = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2

-- c)
cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion _) = 0
cantidadDePuntosVacios (Pasaje m d) = contarVacio m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = contarVacio m + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

contarVacio :: Maybe a -> Int
contarVacio Nothing = 1
contarVacio _ = 0 

-- d)
cantidadDePuntosCon :: Dungeon a -> Int
cantidadDePuntosCon (Habitacion _) = 1
cantidadDePuntosCon (Pasaje m d) = contarLleno m + cantidadDePuntosCon d
cantidadDePuntosCon (Bifurcacion m d1 d2) = contarLleno m + cantidadDePuntosCon d1 + cantidadDePuntosCon d2

contarLleno :: Maybe a -> Int
contarLleno (Just _) = 1
contarLleno _ = 0 

-- e)
esLineal :: Dungeon a -> Bool
esLineal (Habitacion _) = True
esLineal (Pasaje _ d) = esLineal d
esLineal (Bifurcacion _ d1 d2) = False && esLineal d1 && esLineal d2

-- f)
llenoDe :: (Eq a) => a -> Dungeon a -> Bool
llenoDe e (Habitacion e') = e == e'
llenoDe e (Pasaje me d) = maybeEquals e me && llenoDe e d
llenoDe e (Bifurcacion me d1 d2) = maybeEquals e me && llenoDe e d1 && llenoDe e d2

maybeEquals :: (Eq a) => a -> Maybe a -> Bool
maybeEquals e (Just e') = e == e'
maybeEquals _ Nothing = False

