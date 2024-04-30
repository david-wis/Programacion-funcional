module TableroR (TableroR, tableroInicial, mover, poner, sacar, nroBolitas, hayBolitas, puedeMover, boom,
                DirR, ExpR, UOp, BOp, ProgramaR, ComandoR) where

import Rowbstones

tableroInicial :: Int -> TableroR
mover :: DirR -> TableroR -> TableroR
poner :: TableroR -> TableroR
sacar :: TableroR -> TableroR
nroBolitas :: TableroR -> Int
hayBolitas :: TableroR -> Bool
puedeMover :: DirR -> TableroR -> Bool
boom :: String -> TableroR -> a

data TableroR = TableroRCDT Int [Int]

tableroInicial n = TableroRCDT 0 (replicate n 0)

mover dir (TableroRCDT pos row) = if puedeMover dir (TableroRCDT pos row) then TableroRCDT (pos + dirToN dir) row else error "No se puede mover ahi"
                                where dirToN Oeste = -1
                                      dirToN Este  = 1

poner (TableroRCDT pos row) = TableroRCDT pos (applyToElem pos (+1) row)

sacar (TableroRCDT pos row) = TableroRCDT pos (applyToElem pos (\n -> if n == 0 then error "No hay bolitas" else n-1) row)

nroBolitas (TableroRCDT pos row) = row !! pos

hayBolitas t = nroBolitas t > 0

puedeMover Oeste (TableroRCDT pos _) = pos > 0 
puedeMover Este (TableroRCDT pos row) = pos < (length row - 1)

boom msg _ = error msg

applyToElem :: Int -> (a -> a) -> [a] -> [a]
applyToElem _ _ [] = error "Out of bounds"
applyToElem 0 f (x:xs) = f x : xs
applyToElem n f (x:xs) = x : applyToElem (n-1) f xs

instance Show TableroR where
  show (TableroRCDT pos row) = show pos ++ " " ++ show row