
data Pizza = Prepizza | Capa Ingrediente Pizza 

-- Mostrar
instance Show Pizza where
    show Prepizza = "Prepizza"
    show (Capa i p) = show i ++ " -> " ++ show p
--


data Ingrediente = Aceitunas Int | Anchoas | Cebolla
                    | Jamon | Queso | Salsa

-- Mostrar
instance Show Ingrediente where
    show (Aceitunas n) = "Aceitunas (" ++ show n ++ ")"
    show Anchoas = "Anchoas"
    show Cebolla = "Cebolla"
    show Jamon = "Jamon"
    show Queso = "Queso"
    show Salsa = "Salsa"
--

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int 
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa (Aceitunas n) p) = n + cantidadDeAceitunas p
cantidadDeAceitunas (Capa _ p) = cantidadDeAceitunas p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa (Aceitunas n) p) = Capa (Aceitunas (2*n)) (duplicarAceitunas p)
duplicarAceitunas (Capa i p) = Capa i (duplicarAceitunas p)

sinLactosa :: Pizza -> Pizza 
sinLactosa Prepizza = Prepizza
sinLactosa (Capa Queso p) = sinLactosa p 
sinLactosa (Capa i p) = Capa i (sinLactosa p)

aptaIntoLact :: Pizza -> Bool 
aptaIntoLact Prepizza = True
aptaIntoLact (Capa Queso _) = False
aptaIntoLact (Capa _ p) = aptaIntoLact p

conDescripMejorada :: Pizza -> Pizza  
conDescripMejorada Prepizza = Prepizza
conDescripMejorada (Capa (Aceitunas n) p) = juntarAceitunas n (conDescripMejorada p)
conDescripMejorada (Capa i p) = Capa i (conDescripMejorada p)

juntarAceitunas :: Int -> Pizza -> Pizza
juntarAceitunas n (Capa (Aceitunas n') p) = Capa (Aceitunas (n+n')) p
juntarAceitunas n p = Capa (Aceitunas n) p

-- Capa (Aceitunas 5) $ Capa Anchoas $ Capa (Aceitunas 10) $ Capa (Aceitunas 20) $ Capa (Aceitunas 15) $ Capa Jamon Prepizza
