module OutputErrorMonad where

import Monadas
import Control.Monad

type Screen = String    
data OutputError a = OS (a, Screen) | OE (String, Screen) 
    deriving Show

instance Functor OutputError where
    fmap = liftM

instance Applicative OutputError where
    pure x = OS (x, "")
    (<*>) = ap


instance MonadFail  OutputError where
  fail msg = OE (msg, "")

instance Monad OutputError where
    m >>= k = case m of 
                OE (msg, scr) -> OE (msg, scr)
                OS (v, scr1) -> case k v of
                                  OE (msg, scr2) -> OE (msg, scr1++scr2)
                                  OS (res, scr2) -> OS (res, scr1++scr2)


instance PrintMonad OutputError where
    printf msg = OS ((), msg ++ "\n")

instance ErrorMonad OutputError where
    throw msg = OE (msg, "")