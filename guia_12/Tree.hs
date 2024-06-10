import ExpA

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- a)
foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT z f EmptyT = z
foldT z f (NodeT x t1 t2) = f x (foldT z f t1) (foldT z f t2)

-- b)
mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (NodeT . f)

sumT :: Tree Int -> Int
sumT = foldT 0 ((.)(.)(.)(+)(+)) 

sizeT :: Tree a -> Int
sizeT = foldT 0 (const (\s1 s2 -> 1 + s1 + s2))

heightT :: Tree a -> Int
heightT = foldT 0 (const (\s1 s2 -> 1 + max s1 s2))

preOrder :: Tree a -> [a]
preOrder = foldT [] (\x e1 e2 -> x : e1 ++ e2)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\x e1 e2 -> e1 ++ x : e2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\x e1 e2 -> e1 ++ e2 ++ [x])

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (flip . NodeT)

countByT :: (a -> Bool) -> Tree a -> Int
countByT p = foldT 0 (\x e1 e2 -> delta (p x) + e1 + e2)

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT p = foldT ([], []) (\x (l1, l2) (r1, r2) -> if p x 
                                                       then (x : l1 ++ r1, l2 ++ r2) 
                                                       else (l1 ++ r1, x : l2 ++ r2))

zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT (const EmptyT) g
           where g x h1 h2 EmptyT = EmptyT
                 g x h1 h2 (NodeT y t1 t2) = NodeT (f x y) (h1 t1) (h2 t2)


caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] (\x c1 c2 -> if length c1 > length c2 
                                       then x:c1 
                                       else x:c2) 

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [] (\x cs1 cs2 -> [x] : foldr ((:) . (:) x) [] (cs1 ++ cs2))

todosLosCaminos' :: Tree a -> [[a]]
todosLosCaminos' = foldT [] (\x cs1 cs2 -> [x] : map (x:) (cs1 ++ cs2))

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT [] (\x ls1 ls2 -> [x] : merge ls1 ls2)

merge :: [[a]] -> [[a]] -> [[a]]
merge = foldr g id
      where g xs h [] = xs : h []
            g xs h (ys:yss) = (xs ++ ys) : h yss


nivelN :: Tree a -> Int -> [a]
nivelN = foldT (const []) (\x h1 h2 n -> if n == 0 then [x] else h1 (n-1) ++ h2 (n-1))

-- c)
recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT z f EmptyT = z
recT z f (NodeT x t1 t2) = f x t1 t2 (recT z f t1) (recT z f t2)

-- d)
insertT :: (Ord a) => a -> Tree a -> Tree a 
insertT x = recT (NodeT x EmptyT EmptyT) (\y t1 t2 r1 r2 -> if x > y 
                                                            then NodeT y t1 r2 
                                                            else NodeT y r1 t2)

caminoHasta :: (Eq a) => a -> Tree a -> [a]
-- caminoHasta x = recT [] (\y t1 t2 r1 r2 -> if x == y 
--                                            then [x]
--                                            else y : if x `elem` r1 
--                                                     then r1 else r2)
caminoHasta e = foldT [] (\x c1 c2 -> if x == e then [x] else buscarCamino x c1 c2)
              where buscarCamino x [] [] = []
                    buscarCamino x c1 [] = x : c1
                    buscarCamino x _ c2 = x : c2
