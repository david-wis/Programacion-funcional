data EnvBFS t a = EnvT ([t] -> (a, [t])) 

instance Functor (EnvBFS t) where
   fmap = liftM
instance Applicative (EnvBFS t) where
   pure x = EnvT (\s -> (x, s))
   (<*>) = ap 
instance Monad (EnvBFS t) where
    (EnvT f) >>= k = EnvT (\s -> let (x, s') = f s
                                     (EnvT g) = k x in g s')
 

instance Monad (EnvBFS t) where
    store t = EnvT (\ts -> ((), (ts ++ [t])))
    thereIsNext = EnvT (\ts -> (not $ null ts, ts))
    next = EnvT (\(t:ts) -> (t, ts))
    execSeq (EnvT f) = fst $ f []

