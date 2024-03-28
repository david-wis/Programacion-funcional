data Set a = S (a -> Bool)

belongs :: Set a -> a -> Bool
belongs (S p) = p

empty :: Set a
empty = S (const False)

singleton :: (Eq a) => a -> Set a
singleton x = S (== x) 

union :: Set a -> Set a -> Set a
union (S p1) (S p2) = S (\x -> p1 x || p2 x)

intersection :: Set a -> Set a -> Set a
intersection (S p1) (S p2) = S (\x -> p1 x && p2 x)

complement :: Set a -> Set a
complement (S p) = S (not . p)

universe :: Set a
universe = S p'
          where (S p) = empty
                p' = not . p