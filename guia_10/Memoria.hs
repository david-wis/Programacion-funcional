module Memoria (Memoria, enBlanco, cuantoVale, recordar, variables) where

import Data.Map (Map)
import qualified Data.Map as Map
import NExp

-- Testing...
import Data.Char
import Control.Exception (assert)
import Data.Maybe (isNothing)

enBlanco :: Memoria
cuantoVale :: Variable -> Memoria -> Maybe Int
recordar :: Variable -> Int -> Memoria -> Memoria 
variables :: Memoria -> [Variable]

newtype Memoria = MapMemoria (Map Variable Int)
enBlanco = MapMemoria Map.empty
cuantoVale v (MapMemoria m) = Map.lookup v m
recordar v n (MapMemoria m) = MapMemoria $ Map.insert v n m
variables (MapMemoria m) = Map.keys m


----------------------------------------------------------
-- TESTING
----------------------------------------------------------

asignarPrimerosN :: Int -> Memoria -> Memoria 
asignarPrimerosN 0 mem = recordar [chr $ ord 'a'] 0 mem
asignarPrimerosN n mem = recordar [chr $ ord 'a' + n] n (asignarPrimerosN (n-1) mem)


ram = asignarPrimerosN 10 enBlanco
vals = [ (x, cuantoVale x ram) | x <- variables ram ++ ["foo", "bar"] ]

assertTrue True = True
t1 = assertTrue $ length vals == 13 
t2 = assertTrue $ and [ ([chr $ ord 'a' + x], Just x) `elem` vals | x <- [0..9] ]
t3 = assertTrue $ foldr (\v o -> o && isNothing (snd v) ) True (filter (\(x, _) -> x == "foo" || x == "bar") vals)

