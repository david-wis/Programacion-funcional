-- Bonus
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

recr' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr' z f xs = snd (foldr g ([], z) xs)
               where g e (es, r) = (e:es, f e es r)

recr'' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr'' z f = df (foldr rp (const z))
           where df g y = g y y
                 --rp x h [] = f x [] (h [])
                 rp x h (_:ys) = f x ys (h ys)

recr''' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr''' z f xs = appDup (foldr rp (const z) xs) xs
           where appDup g y = g (y, y)
                 --rp x h ([], _) = f x [] (h ([], []))
                 rp x h (_:ys, _) = f x ys (h (ys, ys))

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' = flip recr . (const .)

sumSqrt :: [Int] -> Int
sumSqrt = foldr ((+) . (^2)) 0

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs = foldr ((&&) . flip elem xs) True

subset' :: (Eq a) => [a] -> [a] -> Bool
subset' = foldr (((.)(.)(.) subst (.) (&&)) . elem) (const True)

subst f g x = f x (g x)


accumSum :: [Int] -> [Int]
accumSum = scanr1 (+)

accumSum' :: [Int] -> [Int]
accumSum' = foldr addTerm []
            where addTerm x ys = case ys of
                                    [] -> [x]
                                    (y:_) -> (x+y) : ys
--                  addTerm x [] = [x]
--                  addTerm x (zs@(y:_)) = (x+y) : zs


data DigBin = O | I deriving Show
type NBin = [DigBin]

addNB :: NBin -> NBin -> NBin
addNB n m = addNBWithCarry n m O

addNBWithCarry :: NBin -> NBin -> DigBin -> NBin
addNBWithCarry = foldr foldWC succIf
                       where --foldWC x h [] c = succIf (x : h [] O) c
                             foldWC x h [] c = let (c', d) = addDigBinWithCarry c x O in d :  h [] c'
                             foldWC x h (y:ys) c = let (c', d) = addDigBinWithCarry c x y in d : h ys c'
                             succIf zs O = zs
                             succIf zs I = succNB zs

succNB :: NBin -> NBin
-- succNB = recr [I] (\x xs r -> case x of
--                                O -> I : xs
--                                I -> O : r)
succNB xs = foldr succF (const [I]) xs xs
          where succF _ _ [] = [I]           -- caso innecesario
                succF O _ (_:ys) = I:ys
                succF I h (_:ys) = O:h ys


addDigBinWithCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addDigBinWithCarry O O O = (O, O)
addDigBinWithCarry O O I = (O, I)
addDigBinWithCarry O I O = (O, I)
addDigBinWithCarry O I I = (I, O)
addDigBinWithCarry I O O = (O, I)
addDigBinWithCarry I O I = (I, O)
addDigBinWithCarry I I O = (I, O)
addDigBinWithCarry I I I = (I, I)
--                      

type NU = [()] 
ack :: NU -> NU -> NU
-- ack = recr (():) ackRec
--       where ackRec _ xs h [] = ack xs [()]
--             ackRec x xs h (y:ys) = []

-- ack [] = \ys -> ():ys
-- ack (_:xs) = \ys -> case ys of 
--                       [] -> ack xs [()]
--                       (_:zs) -> ack xs (ack (():xs) zs)

ack = recr (():) (\_ xs h ys -> case ys of 
                              [] -> h [()]
                              (_:zs) -> h (ack (():xs) zs)) 

ack' :: NU -> NU -> NU
ack' = df (foldr rp (\_ r -> ():r))
           where df g y = g y y
                 rp _ h ls@(_:ys) []     = h ys [()]
                 rp _ h (_:ys) (_:zs) = h ys (ack' (():ys) zs)

            --      rp x h (_:ys) = redAck x ys (h ys)
            --      redAck _ _ h [] = h [()]
            --      redAck _ xs h (_:zs) = h (ack' (():xs) zs)


ackInts :: Int -> Int -> Int
ackInts nu1 nu2 = evalNU $ ack' (int2NU nu1) (int2NU nu2)

evalNU :: NU -> Int
evalNU = foldr (const (+1)) 0

int2NU :: Int -> NU
int2NU = flip replicate ()