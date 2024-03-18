compose = (.)
subst f g x = f x (g x)

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = compose f (many (n-1) f)

many' :: Int -> (a -> a) -> (a -> a)
many' 0 = const id
many' n = subst compose (many (n-1))