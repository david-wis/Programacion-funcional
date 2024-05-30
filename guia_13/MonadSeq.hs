{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
import Control.Monad

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
                                                deriving Show

data EnvDFS t a = EnvT ([t] -> (a, [t])) 

class Monad m => MonadSeq m t | m -> t where
    store :: t -> m ()
    thereIsNext :: m Bool
    next :: m t
    execSeq :: m a -> a


instance Functor (EnvDFS t) where
   fmap = liftM
instance Applicative (EnvDFS t) where
   pure x = EnvT (\s -> (x, s))
   (<*>) = ap 
instance Monad (EnvDFS t) where
    (EnvT f) >>= k = EnvT (\s -> let (x, s') = f s
                                     (EnvT g) = k x in g s')
 

instance MonadSeq (EnvDFS t) t where
    store t = EnvT (\ts -> ((), t:ts))
    thereIsNext = EnvT (\ts -> (not $ null ts, ts))
    next = EnvT (\(t:ts) -> (t, ts)) -- next es parcial
    execSeq (EnvT f) = fst $ f []


whileNext :: MonadSeq m t => (t -> m [a]) -> m [a] -> m [a]
whileNext k end = do b <- thereIsNext 
                     if not b 
                     then end 
                     else do t <- next 
                             xs <- k t
                             ys <- whileNext k end
                             return (xs ++ ys)
    
order :: Tree a -> [a]
order t = execSeq (do store t
                      orderM)

orderM :: EnvDFS (Tree a) [a]
orderM = whileNext orderT (return [])


orderT :: Tree a -> EnvDFS (Tree a) [a]
orderT EmptyT = return []
orderT (NodeT x t1 t2) = do store t2
                            store t1
                            return [x]

                                         
ej = NodeT "a" 
                (NodeT "b" 
                        (NodeT "c" EmptyT EmptyT) 
                        (NodeT "d" EmptyT EmptyT)
                )
                (NodeT "e" 
                        (NodeT "f" EmptyT EmptyT) 
                        (NodeT "g" EmptyT EmptyT)
                )