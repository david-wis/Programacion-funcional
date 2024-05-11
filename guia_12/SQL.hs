
-- Ej 4
type Record a b = [(a,b)]
type Table a b = [ Record a b ]

subst f g x = f x (g x)

select :: (Record a b -> Bool) -> Table a b -> Table a b
select = filter

project :: (a -> Bool) -> Table a b -> Table a b
project p = map (filter (p . fst))

conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
conjunct = (.) subst ((&&) .)

crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f = foldr (\x h ys -> map (f x) ys ++ h ys) (const [])

product' :: Table a b -> Table a b -> Table a b
product' = crossWith (++)

similar :: (Eq a, Eq b) => Record a b -> Record a b
similar = foldr (\t r -> if t `elem` r then r else t : r) []


-- Ej 5
data Query a b 
   = Table [Record a b] 
   | Product (Query a b) (Query a b)
   | Projection (a -> Bool) (Query a b)
   | Selection (Record a b -> Bool) (Query a b)

foldQ :: ([Record a b] -> c) -> (c -> c -> c) -> ((a -> Bool) -> c -> c) -> ((Record a b -> Bool) -> c -> c) -> Query a b -> c
foldQ fr _ _ _ (Table r)             = fr r
foldQ fr fpr fpi fs (Product q1 q2)  = fpr (foldQ fr fpr fpi fs q1) (foldQ fr fpr fpi fs q2)
foldQ fr fpr fpi fs (Projection p q) = fpi p (foldQ fr fpr fpi fs q)
foldQ fr fpr fpi fs (Selection p q)  = fs p (foldQ fr fpr fpi fs q)

tables :: Query a b -> [Table a b]
tables = foldQ (:[]) (++) (\_ ts -> ts) (\_ ts -> ts)

execute :: Query a b -> Table a b
execute = foldQ id product' project select

compact :: Query a b -> Query a b
compact = foldQ Table Product compactProjection compactSelection
        where compactProjection p1 (Projection p2 q) = Projection (conjunct p1 p2) q
              compactProjection p q = Projection p q
              compactSelection p1 (Selection p2 q) = Selection (conjunct p1 p2) q
              compactSelection p q = Selection p q

