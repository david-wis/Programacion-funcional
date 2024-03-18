compose = (.)

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = compose f (many (n-1) f)
