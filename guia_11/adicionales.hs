-- Adicional clase 11
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

recr' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr' z f xs = snd (foldr g ([], z) xs)
               where g e (es, r) = (e:es, f e es r)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f = flip recr (const . f)