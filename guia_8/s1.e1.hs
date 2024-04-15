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