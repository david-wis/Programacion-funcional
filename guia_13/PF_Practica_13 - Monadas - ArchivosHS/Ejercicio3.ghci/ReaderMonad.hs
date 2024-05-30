module ReaderMonad where

-- Una implementación de una mónada Reader
import Monadas
import Control.Monad.Fail

data Reader r a = R (r -> a)

instance Functor (Reader r) where
   fmap f (R g) = R (f . g)

instance Applicative (Reader r) where
   pure x            = R (\_ -> x)
   (R hf) <*> (R hx) = R (\r -> hf r (hx r))

instance Monad (Reader r) where
   m >>= k  = R (\r -> 
                   let R fm = m
                    in let R fk = k (fm r)
                        in fk r)

instance ReaderMonad r (Reader r) where
  ask = R (\r -> r)
  runRM m r = let R fm = m
               in fm r