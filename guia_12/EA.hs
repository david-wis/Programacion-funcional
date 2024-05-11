import ExpA

data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show

data ABTree a b = Leaf b | Node a (ABTree a b) (ABTree a b)

-- a)
foldEA :: (Int -> a) -> (BinOp -> a -> a -> a) -> EA -> a
foldEA f g (Const n) = f n
foldEA f g (BOp bop e1 e2) = g bop (foldEA f g e1) (foldEA f g e2)

-- b)
noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (>= 0) (const (&&))

simplificarEA :: EA -> EA
simplificarEA = foldEA Const elimOp
              where elimOp Sum = elimSumImprEA
                    elimOp Mul = elimProdImprEA

elimSumImprEA :: EA -> EA -> EA 
elimSumImprEA (Const 0) e2 = e2
elimSumImprEA e1 (Const 0) = e1
elimSumImprEA e1 e2 = BOp Sum e1 e2

elimProdImprEA :: EA -> EA -> EA 
elimProdImprEA (Const 0) _ = Const 0
elimProdImprEA _ (Const 0) = Const 0
elimProdImprEA (Const 1) e2 = e2
elimProdImprEA e1 (Const 1) = e1
elimProdImprEA e1 e2 = BOp Mul e1 e2

evalEA' :: EA -> Int
evalEA' = foldEA id bopToOp
        where bopToOp Sum = (+)
              bopToOp Mul = (*)

showEA :: EA -> String
showEA = foldEA show (showOp . bopToS)
       where bopToS Sum = "+"
             bopToS Mul = "*"
             showOp op e1 e2 = "(" ++ e1 ++ op ++ e2 ++ ")"

ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte bopToExpA
         where bopToExpA Sum = Suma
               bopToExpA Mul = Prod

ea2Arbol' :: EA -> ABTree BinOp Int
ea2Arbol' = foldEA Leaf Node


