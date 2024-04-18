import S2


data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA ExpA

-- i)

evalExpA :: ExpA -> Int
evalExpA (Cte n) = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2


-- ii)

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n) = Cte n
simplificarExpA (Suma e1 e2) = elimSumImpr (simplificarExpA e1)  (simplificarExpA e2)
simplificarExpA (Prod e1 e2) = elimProdImpr (simplificarExpA e1)  (simplificarExpA e2)

elimSumImpr :: ExpA -> ExpA -> ExpA 
elimSumImpr (Cte 0) e2 = e2
elimSumImpr e1 (Cte 0) = e1
elimSumImpr e1 e2 = Suma e1 e2

elimProdImpr :: ExpA -> ExpA -> ExpA 
elimProdImpr (Cte 0) _ = Cte 0
elimProdImpr _ (Cte 0) = Cte 0
elimProdImpr (Cte 1) e2 = e2
elimProdImpr e1 (Cte 1) = e1
elimProdImpr e1 e2 = Prod e1 e2

-- iii)
cantidadSumaCero :: ExpA -> Int
cantidadSumaCero (Cte _) = 0 
cantidadSumaCero (Suma e1 e2) = contarCeros e1 e2 + cantidadSumaCero e1 + cantidadSumaCero e2
cantidadSumaCero (Prod e1 e2) = cantidadSumaCero e1 + cantidadSumaCero e2

contarCeros :: ExpA -> ExpA -> Int
contarCeros (Cte 0) _ = 1
contarCeros _ (Cte 0) = 1
contarCeros _ _ = 0

-------------------------------------------------

data ExpS = CteS N
            | SumS ExpS ExpS
            | ProdS ExpS ExpS
            deriving Show
            
evalES :: ExpS -> Int
evalES (CteS n) = evalN n 
evalEs (SumS e1 e2) = evalES e1 + evalES e2
evalEs (ProdS e1 e2) = evalES e1 * evalES e2

es2ExpA :: ExpS -> ExpA
es2ExpA (CteS n) = Cte (evalN n)
es2ExpA (SumS e1 e2) = Suma (es2ExpA e1) (es2ExpA e2)
es2ExpA (ProdS e1 e2) = Prod (es2ExpA e1) (es2ExpA e2)


