-- Adicional clase 11
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

recr' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr' z f xs = snd (foldr g ([], z) xs)
               where g e (es, r) = (e:es, f e es r)

recr'' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr'' z f = df (foldr rp (const z))
           where df g y = g y y
                 rp x h [] = f x [] (h [])
                 rp x h (_:ys) = f x ys (h ys)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f = flip recr (const . f)