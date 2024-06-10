data GTree a = GNode a [GTree a] deriving Show

delta :: Bool -> Int
delta True = 1
delta False = 0

foldGT0 :: (a -> [b] -> b)                     -> GTree a -> b
foldGT1 :: (a -> c -> b) -> (b -> c -> c) -> c -> GTree a -> b
foldGT  :: (a -> c -> b) ->     ([b] -> c)     -> GTree a -> b

foldGT0 h (GNode x ts) = h x (map (foldGT0 h) ts)

foldGT1 g f z (GNode x ts) = g x (foldr f z (map (foldGT1 g f z) ts))

foldGT g k (GNode x ts) = g x (k (map (foldGT g k) ts))


-- ???
-- recGT0 :: (a -> [b] -> b)                     -> GTree a -> b
-- recGT1 :: (a -> c -> b) -> (b -> c -> c) -> c -> GTree a -> b
-- recGT  :: (a -> c -> b) ->     ([b] -> c)     -> GTree a -> b

mapGT :: (a -> b) -> GTree a -> GTree b
mapGT f = foldGT (GNode . f) id

sumGT :: GTree Int -> Int
sumGT = foldGT (+) sum

sizeGT :: GTree a -> Int
sizeGT = sumGT . mapGT (const 1)

heightGT :: GTree a -> Int
heightGT = foldGT (const (+1)) maximumHeight
         where maximumHeight [] = 0
               maximumHeight xs = maximum xs

preOrderGT :: GTree a -> [a]
preOrderGT = foldGT (:) concat

inOrderGT :: GTree a -> [a]
inOrderGT = foldGT putSecond id
          where putSecond x [] = [x]
                putSecond x (y:ys) = y ++ [x] ++ concat ys

postOrderGT :: GTree a -> [a]
postOrderGT = foldGT (\x xs -> xs ++ [x]) concat

mirrorGT :: GTree a -> GTree a
mirrorGT = foldGT GNode reverse

countByGT :: (a -> Bool) -> GTree a -> Int
countByGT p = foldGT ((+) . (delta . p)) sum

partitionGT :: (a -> Bool) -> GTree a -> ([a], [a])
partitionGT p = foldGT (\x (ls, rs) -> if p x then (x : ls, rs) else (ls, x : rs)) 
                       (foldr (\(ls, rs) (ls', rs') -> (ls ++ ls', rs ++ rs')) ([], []))

-- foldGT  :: (a -> c -> b) ->     ([b] -> c)     -> GTree a -> b
-- foldGT g k (GNode x ts) = g x (k (map (foldGT g k) ts))


zipWithGT :: (a->b->c) -> GTree a -> GTree b -> GTree c
zipWithGT f = foldGT g id
            where g x hs (GNode y ts) = GNode (f x y) (zipWith ($) hs ts)

caminoMasLargo :: GTree a -> [a]
caminoMasLargo = foldGT (\x (_, xs) -> x:xs) (maximumByOrder length (0, []))

maximumByOrder :: (Ord b) => (a -> b) -> (b, a) -> [a] -> (b, a)
maximumByOrder f = foldr (\x (m, mx) -> if f x > m then (f x, x) else (m, mx))

todosLosCaminosGT :: GTree a -> [[a]]
todosLosCaminosGT = foldGT (\x xss -> [x] : map (x:) xss) concat

todosLosNivelesGT :: GTree a -> [[a]]
todosLosNivelesGT = foldGT (\x xss -> [x]:xss) (foldr merge [])

merge :: [[a]] -> [[a]] -> [[a]]
merge = foldr g id
      where g xs h [] = xs : h []
            g xs h (ys:yss) = (xs ++ ys) : h yss

caminoHastaGT :: Eq a => a -> GTree a -> [a]
caminoHastaGT e = foldGT (\x xs -> if e == x then [x] else x:xs) 
                         (foldr (\xs r -> if e `elem` xs then xs else r) [])

caminoHastaGT' :: Eq a => a -> GTree a -> [a]
caminoHastaGT' e = foldGT addIfExists buscarCamino
                  where buscarCamino = foldr (\xs rs -> retIfEmpty xs rs xs) []
                        addIfExists x xs = if x == e then [x]
                                                     else retIfEmpty xs [] (x:xs)
                        retIfEmpty [] x1 _ = x1
                        retIfEmpty (_:_) _ x2 = x2

nivelNGT :: GTree a -> Int -> [a]
nivelNGT = foldGT g id
         where g x _ 0 = [x]
               g _ hs n = concat (map ($ n-1) hs)

t1 a b c d = GNode a [GNode b [], GNode c [], GNode d []]
t2 = GNode 4 [t1 1 0 2 3 , GNode 5 [], GNode 7 [GNode 6 [], GNode 8 []]]
