module EA where 

data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA ExpA
          deriving Show

data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show

evalEA :: EA -> Int
evalEA (Const n) = n
evalEA (BOp b e1 e2) = case b of 
                         Sum -> evalEA e1 + evalEA e2
                         Mul ->  evalEA e1 * evalEA e2

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp b e1 e2) = case b of 
                          Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
                          Mul -> Prod (ea2ExpA e1) (ea2ExpA e2)


expA2ea :: ExpA -> EA 
expA2ea (Cte n) = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)

------------------------------------------------------------
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _) = 1
cantidadDeHojas (Nodo _ a1 a2) = cantidadDeHojas a1 + cantidadDeHojas a2

cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _) = 0
cantidadDeNodos (Nodo _ a1 a2) = 1 + cantidadDeNodos a1 + cantidadDeNodos a2

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _) = 1
cantidadDeConstructores (Nodo _ a1 a2) = 1 + cantidadDeConstructores a1 + cantidadDeConstructores a2

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n) = Hoja n
ea2Arbol (BOp b t1 t2) = Nodo b (ea2Arbol t1) (ea2Arbol t2)

------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT e t1 t2) = e + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

anyT :: (a -> Bool) -> Tree a -> Bool
anyT _ EmptyT = False
anyT p (NodeT e t1 t2) = p e || anyT p t1 ||  anyT p t2

countT :: (a -> Bool) -> Tree a -> Int
countT _ EmptyT = 0
countT p (NodeT e t1 t2) = delta (p e) + countT p t1 + countT p t2
                            where delta False = 0
                                  delta True = 1

countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (NodeT _ EmptyT EmptyT) = 1
countLeaves (NodeT _ t1 t2) = countLeaves t1 + countLeaves t2


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT e t1 t2) = inOrder t1 ++ [e] ++ inOrder t2


-- V1

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT e t1 t2) = [e] : merge (listPerLevel t1) (listPerLevel t2)

-- merge :: [[a]] -> [[a]] -> [[a]]
-- merge [] [] = []
-- merge (xs:xss) [] = xs : merge xss []
-- merge [] (ys:yss) = ys : merge [] yss
-- merge (xs:xss) (ys:yss) = (xs++ys) : merge xss yss


merge :: [[a]] -> [[a]] -> [[a]]
merge [] yss = yss
merge (xs:xss) (ys:yss) = (xs++ys) : merge xss yss
merge xss [] = xss

-- V2 ("HORRIBLE")

listPerLevel' :: Tree a -> [[a]]
listPerLevel' t = listPerLevelWGuide t [] 0

listPerLevelWGuide :: Tree a -> [[a]] -> Int -> [[a]]
listPerLevelWGuide EmptyT ess _ = ess
listPerLevelWGuide (NodeT e t1 t2) ess n = listPerLevelWGuide t2 l (n+1)
                                            where l = rebuildListWithElem n (listPerLevelWGuide t1 ess (n+1)) e

rebuildListWithElem :: Int -> [[a]] -> a -> [[a]]
rebuildListWithElem 0 [] e = [[e]]
rebuildListWithElem 0 (es:ess) e = (es ++ [e]):ess
rebuildListWithElem n [] e = [] : rebuildListWithElem (n-1) [] e
rebuildListWithElem n (es:ess) e = es : rebuildListWithElem (n-1) ess e


-- Precond: recibe un BST
insertBST :: (Ord a) => a -> Tree a -> Tree a 
insertBST e EmptyT = NodeT e EmptyT EmptyT
insertBST e (NodeT e' t1 t2) = if e < e'
                               then NodeT e' (insertBST e t1) t2
                               else NodeT e' t1 (insertBST e t2) 

--listPerLevel $ insertBST 7 $ insertBST 3 $ insertBST 1 $ insertBST 2 $ insertBST 6 $ insertBST 4 $ insertBST 5 $ EmptyT


mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT e t1 t2) = NodeT e (mirrorT t2) (mirrorT t1)


levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT e _ _) = [e]
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2


ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT e t1 t2) = if length l1 > length l2
                               then e : l1
                               else e : l2
                               where l1 = ramaMasLarga t1
                                     l2 = ramaMasLarga t2


todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT e t1 t2) = [e] : appendToAll e (todosLosCaminos t1 ++ todosLosCaminos t2)

appendToAll :: a -> [[a]] -> [[a]]
appendToAll _ [] = []
appendToAll x (xs:xss) = (x:xs) : appendToAll x xss

