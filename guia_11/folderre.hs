sum' :: [Int] -> Int
sum' = foldr (+) 0

length' :: [a] -> Int
-- length' = foldr (\x l -> l + 1) 0
length' = foldr (const (+1)) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ms -> f x : ms) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

find' :: (a -> Bool) -> [a] -> Maybe a
find' = flip (foldr (\x r p -> if p x then Just x else r p) (const Nothing))

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x b -> p x || b) False

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x b -> p x && b) True

delta :: Bool -> Int
delta True = 1
delta False = 0

countBy' :: (a -> Bool) -> [a] -> Int
-- countBy' p = foldr (\x n -> delta (p x) + n) 0
countBy' p = foldr ((+) . delta . p) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr (\x (xs, ys) -> if p x then (x:xs, ys) else (xs, x:ys)) ([], [])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = flip (foldr (\x h f ys -> case ys of 
                                      [] -> []
                                      (y':ys') -> f x y' : h f ys') (\_ _ -> []))


flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z = f z y x

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
-- scanr' = flip3 (foldr (\x h z f -> let (n:ns) = h z f 
--                                    in f x n : (n:ns)) (\z _ -> [z]))
scanr' f z = foldr (\x rs@(r:_) -> f x r : rs) [z]


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x rs -> if p x then x:rs else []) []

take' :: Int -> [a] -> [a]
take' = flip (foldr (\x h n -> if n == 0 then [] else x : h (n-1)) (const []))

drop' :: Int -> [a] -> [a]
drop' = flip (foldr (\x h n -> if n == 0 then x : h n else h (n-1)) (const [])) 

(!!?) :: Int -> [a] -> a
(!!?) = flip (foldr (\x h n -> if n == 0 then x else h (n-1)) (error ""))
