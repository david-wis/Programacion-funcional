data Pizza = Prepizza | Capa Ingrediente Pizza 

-- Mostrar
instance Show Pizza where
    show Prepizza = "Prepizza"
    show (Capa i p) = show i ++ " -> " ++ show p
--


data Ingrediente = Aceitunas Int | Anchoas | Cebolla
                    | Jamon | Queso | Salsa deriving Eq

-- Mostrar
instance Show Ingrediente where
    show (Aceitunas n) = "Aceitunas (" ++ show n ++ ")"
    show Anchoas = "Anchoas"
    show Cebolla = "Cebolla"
    show Jamon = "Jamon"
    show Queso = "Queso"
    show Salsa = "Salsa"

delta :: Bool -> Int
delta True = 1
delta False = 0

-- Ej 1
-- a)
cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen p (Capa i pizza) = delta (p i) + cantidadCapasQueCumplen p pizza

-- b)
conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i pizza) = Capa (f i) (conCapasTransformadas f pizza)

-- c)
soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza = Prepizza
soloLasCapasQue p (Capa i pizza) = if p i then Capa i (soloLasCapasQue p pizza) 
                                          else soloLasCapasQue p pizza

-- Ej 2
sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (/= Queso) 

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = (== 0) . cantidadCapasQueCumplen (== Queso) 

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen (== Queso) 

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas duplicarAceitunas
                        where duplicarAceitunas (Aceitunas n) = Aceitunas (2*n)
                              duplicarAceitunas i = i


-- Ej 3
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada _ z Prepizza = z
pizzaProcesada f z (Capa i p) = f i (pizzaProcesada f z p)

-- Ej 4
cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' p = pizzaProcesada (\i n -> delta (p i) + n) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (Capa . f) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' p = pizzaProcesada (\i pizza -> if p i then Capa i pizza else pizza) Prepizza


-- Ej 5
cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i n -> case i of 
                                              Aceitunas n' -> n+n'
                                              _ -> n) 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen p = pizzaProcesada (\i is -> if p i then i:is else is) []

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada comprimirAceitunas Prepizza
                         where comprimirAceitunas (Aceitunas n) (Capa (Aceitunas n') pizza) = Capa (Aceitunas (n+n')) pizza
                               comprimirAceitunas i pizza = Capa i pizza

conCapasDe :: Pizza -> Pizza -> Pizza
-- Origen:
-- conCapasDe = pizzaProcesada (\i h -> \p -> Capa i (h p)) id
conCapasDe = pizzaProcesada ((.) . Capa) id

-- Otra version
-- conCapasDe' p1 p2 = pizzaProcesada Capa p2 p1


primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (pizzaProcesada (\i f n -> if n == 0 then Prepizza else Capa i (f (n-1))) (const Prepizza))