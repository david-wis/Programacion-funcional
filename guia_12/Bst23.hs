import Data.List (intercalate)
data BST23 a = Cero
             | Dos Int Int (BST23 a) a (BST23 a)
             | Tres Int Int (BST23 a) a (BST23 a) a (BST23 a) -- deriving Show

-- instance Show a => Show (BST23 a) where
--     show Cero = ""
--     show (Dos _ _ izq valor der) = "(Dos " ++ show izq ++ " " ++ show valor ++ " " ++ show der ++ ")"
--     show (Tres _ _ izq1 valor1 izq2 valor2 der) = "(Tres " ++ show izq1 ++ " " ++ show valor1 ++ " " ++ show izq2 ++ " " ++ show valor2 ++ " " ++ show der ++ ")"

-- Dos 3 4 (Dos 3 2 (Dos 1 1 Cero (-7) Cero) (-5) (Dos 1 1 Cero (-3) Cero)) (-1) (Dos 1 2 (Dos 1 1 Cero 2 Cero) 4 (Dos 1 1 Cero 6 Cero))

-- perLevel :: BST23 a -> [[a]]
-- perLevel = fold23 [] (\h s r1 x r2 -> [x] : merge r1 r2) (\h s r1 x r2 y r3 -> [x,y]: merge r1 (merge r2 r3))


-- showPerLevel :: Show a =>  BST23 a -> [[String]]
-- showPerLevel = fold23 [] (\h s r1 x r2 -> ["Dos (" ++ show h  ++ ", " ++ show s ++ "): " ++ show x]: merge r1 r2) (\h s r1 x r2 y r3 -> ["Tres (" ++ show h ++ ", " ++ show s ++ "): " ++show x++" , "++show y]: merge r1 (merge r2 r3))

merge :: [[a]] -> [[a]] -> [[a]]
merge = foldr g id
      where g xs h [] = xs : h []
            g xs h (ys:yss) = (xs ++ ys) : h yss

-- instance Show a => Show (BST23 a) where
--     show t = intercalate "; " (intercalate ["\n"] (showPerLevel t))

showPerLevel :: Show a =>  BST23 a -> [String]
showPerLevel = fold23 [] showDos showTres
                   where showDos 1 s r1 x r2 = ["/D(" ++ show x ++ ")\\"]
                         showDos h s r1 x r2 = let sx = "D(" ++ show x ++ ")"
                                                   l1 = length (head r1)
                                                   l2 = length (head r2)
                                                   lx = length sx
                                                   pad1 = "/" ++ replicate (l1 - div lx 2 - 1) ' '
                                                   pad2 = replicate (l1 + l2 - lx - length pad1 - 1) ' ' ++ "\\"
                                                   in (pad1 ++ sx ++ pad2) : merge r1 r2
                         showTres 1 s r1 x r2 y r3 = ["/T(" ++ show x ++ "," ++ show y ++ ")\\"]
                         showTres h s r1 x r2 y r3 = let sx = "T(" ++ show x
                                                         sy = "," ++ show y ++ ")"
                                                         l1 = length (head r1)
                                                         l2 = length (head r2)
                                                         l3 = length (head r3)
                                                         pad1 = "/" ++ replicate (l1-4) ' '
                                                         pad2 = replicate (l2 - length sx) ' '
                                                         pad3 = replicate (l3 - length sy + 2) ' ' ++ "\\"
                                                         in ( pad1 ++ sx ++ pad2 ++ sy ++ pad3) : merge r1 (merge r2 r3)

instance Show a => Show (BST23 a) where
    show t = intercalate "\n" (showPerLevel t)

t = foldr insert Cero (map (\x -> if x `mod` 2 == 0 then x else (-1)*x) [1..20])
ts = foldl (flip insert) Cero (map (\x -> if x `mod` 2 == 0 then x else (-1)*x) [1..20])

-- process = putStr $ intercalate "\n\n\n" (map show ts)

---------------------------------------------------------


fold23 :: b -> (Int -> Int -> b -> a -> b -> b) -> (Int -> Int -> b -> a -> b -> a -> b -> b) -> BST23 a -> b
fold23 z f g Cero = z
fold23 z f g (Dos h n t1 x t2) = f h n (fold23 z f g t1) x (fold23 z f g t2)
fold23 z f g (Tres h n t1 x t2 y t3) = g h n (fold23 z f g t1) x (fold23 z f g t2) y (fold23 z f g t3)

rec23 :: b -> (Int -> Int -> BST23 a -> BST23 a -> b -> a -> b -> b) -> (Int -> Int -> BST23 a -> BST23 a -> BST23 a -> b -> a -> b -> a -> b -> b) -> BST23 a -> b
rec23 z f g Cero = z
rec23 z f g (Dos h n t1 x t2) = f h n t1 t2 (rec23 z f g t1) x (rec23 z f g t2)
rec23 z f g (Tres h n t1 x t2 y t3) = g h n t1 t2 t3 (rec23 z f g t1) x (rec23 z f g t2) y (rec23 z f g t3)

-- a)
inOrder :: BST23 a -> [a]
inOrder = fold23 [] (\_ _ ls x rs -> ls ++ [x] ++ rs) 
                    (\_ _ ls x ms y rs -> ls ++ [x] ++ ms ++ [y] ++ rs)
                

-- b)
cantElem :: BST23 a -> Int
cantElem = fold23 0 (\_ _ ln _ rn -> 1 + ln + rn) (\_ _ ln _ mn _ rn -> 2 + ln + mn + rn)

-- c)
height :: BST23 a -> Int
height = fold23 0 (\_ _ ln _ rn -> 1 + max ln rn) (\_ _ ln _ mn _ rn -> 1 + max (max ln mn) rn)

-- d)
--opElem :: ([a] -> a) -> BST23 a -> a
--opElem op = rec23 (error "Arbol vacio") 
--                (\_ _ t1 t2 ln x rn -> op (returnIfCero t1 ln ++ [x] ++ returnIfCero t2 rn))
--                (\_ _ t1 t2 t3 ln x mn y rn -> op (returnIfCero t1 ln ++ [x] ++ returnIfCero t2 mn ++ [y] ++ returnIfCero t3 rn))
--        where returnIfCero Cero _ = []
--              returnIfCero t n = [n]
--
--minElem :: Ord a => BST23 a -> a
--minElem = opElem minimum
--
-- e)
--maxElem :: Ord a => BST23 a -> a
--maxElem = opElem maximum

-- d)

minElem :: BST23 a -> a
minElem = rec23 (error "Arbol vacio") 
                (\_ _ t1 _ ln x _ -> returnIfCero t1 ln x)
                (\_ _ t1 _ _ ln x _ _ _ -> returnIfCero t1 ln x)
        where returnIfCero Cero _ x = x
              returnIfCero t n _ = n

maxElem :: BST23 a -> a
maxElem = rec23 (error "Arbol vacio") 
                (\_ _ _ t2 _ x rn -> returnIfCero t2 rn x)
                (\_ _ _ _ t3 _ _ _ y rn -> returnIfCero t3 rn y)
        where returnIfCero Cero _ x = x
              returnIfCero t n _ = n

search :: Ord a => a -> BST23 a -> Maybe a
search e = fold23 Nothing
                (\_ _ l x r -> case compare e x of
                                 EQ -> Just x
                                 LT -> l
                                 GT -> r)
                (\_ _ l x m y r -> case compare e x of
                                     EQ -> Just x
                                     LT -> l
                                     GT -> case compare e y of 
                                             EQ -> Just y
                                             LT -> m
                                             GT -> r)

insert :: Ord a => a -> BST23 a -> BST23 a  
insert e = rec23 (armarHojaDos e) insertD insertT
         where 
               insertD 1 _ _ _ _ x _ = armarHojaTres x e                                            -- Dos aguanta un elemento más
               insertD h n t1 t2 l x r = if e < x then                                                -- Hay que ver si me fumo la explosion o no
                                                        if exploded t1 l then mergeDos l h n x t2
                                                                         else Dos h (n+1) l x t2
                                                  else 
                                                        if exploded t2 r then mergeDos r h n x t1
                                                                         else Dos h (n+1) t1 x r
               insertT 1 _ _ _ _ _ x _ y _ = Dos 2 3 (armarHojaDos $ minimum [x,y,e]) (medio x y e) (armarHojaDos $ maximum [x,y,e]) -- Exploto
               insertT h n t1 t2 t3 l x m y r | e < x = if not (exploded t1 l) then Tres h (n+1) l x t2 y t3 -- Propago
                                                        else armarDos l x (armarDos t2 y t3)                 -- Rearmo
                                              | e < y = if not (exploded t2 m) then Tres h (n+1) t1 x m y t3
                                                        else romperMedio t1 x y t3 m
                                              | otherwise = if not (exploded t3 r) then Tres h (n+1) t1 x t2 y r
                                                            else armarDos (armarDos t1 x t2) y r

               exploded (Tres {}) (Dos {}) = True
               exploded _ _ = False

armarDos :: Ord a => BST23 a -> a -> BST23 a -> BST23 a
armarDos t1 x t2 = Dos (1 + max (altura t1) (altura t2)) (1 + nodos t1 + nodos t2) t1 x t2

romperMedio :: Ord a => BST23 a -> a -> a -> BST23 a -> BST23 a -> BST23 a 
romperMedio t1 x y t3 (Dos _ _ tm1 z tm2) = armarDos (armarDos t1 x tm1) z (armarDos tm2 y t3)

altura :: BST23 a -> Int
altura Cero = 0
altura (Dos h _ _ _ _ ) = h
altura (Tres h _ _ _ _ _ _) = h

nodos :: BST23 a -> Int
nodos Cero = 0
nodos (Dos _ n _ _ _ ) = n
nodos (Tres _ n _ _ _ _ _) = n

mergeDos :: Ord a => BST23 a -> Int -> Int -> a -> BST23 a -> BST23 a
mergeDos (Dos _ _ t1 x t2) h n y told = if x < y then Tres h (n+1) t1 x t2 y told
                                                 else Tres h (n+1) told y t1 x t2

prim :: Ord a => BST23 a -> a -> a
prim (Tres _ _ t1 x t2 y t3) z = minimum [x,y,z]

ult :: Ord a => BST23 a -> a -> a
ult (Tres _ _ t1 x t2 y t3) z = maximum [x,y,z]


medioConArbol :: Ord a => BST23 a -> a -> a
medioConArbol (Tres _ _ t1 x t2 y t3) = medio x y

medio :: Ord a => a -> a -> a -> a
medio x y z = let xs = [x,y,z]
                  mi = minimum xs
                  ma = maximum xs
                  in foldr1 (\e r -> if e /= mi && e /= ma then e else r) xs


armarHojaDos :: a -> BST23 a
armarHojaDos x = Dos 1 1 Cero x Cero

armarHojaTres :: Ord a => a -> a -> BST23 a
armarHojaTres x y = if x < y 
                then Tres 1 2 Cero x Cero y Cero
                else Tres 1 2 Cero y Cero x Cero

               
