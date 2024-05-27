import Control.Monad

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
                                                deriving Show

-- numT :: MonadNum m => Tree a -> Tree (Int, a)
numT :: Tree a -> Tree (Int, a)
-- numT t = execNE (numTM t) 
numT t = execNEDFS (numTM t) 


numTM :: MonadNum m => Tree a -> m (Tree (Int, a))
numTM EmptyT = return EmptyT
numTM (NodeT e t1 t2) = do tt1 <- numTM t1
                           incNum
                           v <- getNum
                           tt2 <- numTM t2
                           return $ NodeT (v, e) tt1 tt2

class Monad m => MonadNum m where
  getNum :: m Int
  incNum :: m ()
  execNE :: m a -> a
  -- debugNE :: m a -> Int
  -- execNE getNum = 0
  -- execNE incNum = ()
  -- execNE (incNum >>= \_ -> getNum) = 1
  -- execNE (do incNum
  --            n <- getNum
  --            incNum
  --            m <- getNum
  --            return (n+m)) = 3


data NumEnvDFS a = EnvT (Int -> (a, Int)) 

instance Functor NumEnvDFS where
    fmap = liftM

instance Applicative NumEnvDFS where
    (<*>) = ap
    pure x = EnvT (\n -> (x, n))

instance Monad NumEnvDFS where
    -- return x = EnvT (\n -> (x, n))
    (EnvT f) >>= k = EnvT (\n -> let (x, n') = f n
                                     (EnvT g) = k x in g n')

instance MonadNum NumEnvDFS where
    getNum = EnvT (\n -> (n, n))
    incNum = EnvT (\n -> ((), n+1))
    execNE (EnvT f) = fst $ f 0
    -- debugNE (EnvT f) = snd $ f 0


-- Para poder correrlo:
execNEDFS :: NumEnvDFS a -> a
execNEDFS = execNE

-- debugNEDFS :: NumEnvDFS a -> Int
-- debugNEDFS = debugNE

test = execNEDFS (do incNum
                     n <- getNum
                     incNum
                     m <- getNum
                     return (n+m))