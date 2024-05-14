data BST23 a = Cero
             | Dos Int Int (BST23 a) a (BST23 a)
             | Tres Int Int (BST23 a) a (BST23 a) a (BST23 a)

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
cantElem = fold23 0 (\_ _ ln _ rn -> 1 + ln + rn) (\_ _ ln _ mn _ rn -> 1 + ln + mn + rn)

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

--insert :: a -> BST a -> BST a  
--insert e = rec23 (Dos 1 1 Cero a Cero) insertD insertT
--         where insertD 1 n tl t2 l x r = if x > e 
--                                        then Tres 1 2 Cero e Cero x Cero
--                                        else Tres 1 2 Cero x Cero e Cero
--               insertD h n tl t2 l x r = if x > e then
--                                                    if exploded t1 l then Tres h (n+1) l (armarDos )
--                                         
--                                         
--               insertT 1 n t1 t2 t3 l x m y r = Dos 1 1 Cero (maximum [x, y, a]) Cero
--               insertT h n t1 t2 t3 l x m y r = 
--
--               exploded Tres Dos = True
--               exploded _ _ = False
--
--               armarDos x = Dos 1 1 Cero x Cero
--
               
            

