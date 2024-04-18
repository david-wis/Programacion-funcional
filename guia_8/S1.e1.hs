-- a) RE
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- b) RE
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- c) RE
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

-- d) RE
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- e) RE
elem' :: (Eq a) => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = x == e || elem' e xs

-- f) RE
all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = p x && all' p xs

-- g) RE
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = p x || any' p xs

-- h) RE
count' :: (a -> Bool) -> [a] -> Int
count' p [] = 0
count' p (x:xs) = plusOneIf p x + count' p xs
                    where plusOneIf p' x' = if p' x' then 1 else 0

-- i) RE
subset :: Eq a => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = x `elem` ys && subset xs ys

-- j) RE
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- k) RE
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- l)
-- Usando recursion doble
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _  = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- RE
zip'' :: [a] -> [b] -> [(a,b)]
zip'' [] _ = []
zip'' (x:xs) ys = if null ys then [] else (x, head ys) : zip'' xs (tail ys)

-- m) RE?
unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' (x:xs) = let (a, b) = x 
                    (as, bs) = unzip' xs
                    in (a:as, b:bs)


laMasLarga :: [[a]] -> [a]
laMasLarga [] = error "no hay lista"
laMasLarga [xs] = xs
laMasLarga (xs:xss) = let m = laMasLarga xss in 
                      if length xs > length m
                      then xs
                      else m
    
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy _ _ [] = False
elemBy p e (x:xs) = p e x || elemBy p e xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n xs = case xs of 
               (y:ys) -> drop' (n-1) ys
               _ -> []

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) = if p x 
                      then dropWhile' p xs
                      else x:xs
                      

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x 
                      then x : takeWhile' p xs
                      else []







