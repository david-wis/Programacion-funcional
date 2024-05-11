module ExpA where

data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA ExpA deriving Show

delta :: Bool -> Int
delta True = 1
delta False = 0

--a)

foldExpA :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> ExpA -> a
foldExpA f _ _ (Cte n) = f n
foldExpA f g h (Suma e1 e2) = g (foldExpA f g h e1) (foldExpA f g h e2)
foldExpA f g h (Prod e1 e2) = h (foldExpA f g h e1) (foldExpA f g h e2)


-- b)
cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA (delta . (==0)) (+) (+)

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (>=0) (&&) (&&)

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA Cte elimSumImpr elimProdImpr

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

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

showExpA :: ExpA -> String
showExpA = foldExpA show (showOp "+") (showOp "*")
         where showOp op e1 e2 = "(" ++ show e1 ++ op ++ show e2 ++ ")"

-- d)
recExpA :: (Int -> a) -> (ExpA -> ExpA -> a -> a -> a) -> (ExpA -> ExpA -> a -> a -> a) -> ExpA -> a
recExpA f _ _ (Cte n) = f n
recExpA f g h (Suma e1 e2) = g e1 e2 (recExpA f g h e1) (recExpA f g h e2)
recExpA f g h (Prod e1 e2) = h e1 e2 (recExpA f g h e1) (recExpA f g h e2)


-- e)
cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA (const 0) contarSuma0 (const $ const (+))
                where contarSuma0 (Cte 0) _ r1 r2 = 1 + r1 + r2
                      contarSuma0 _ (Cte 0) r1 r2 = 1 + r1 + r2
                      contarSuma0 _ _ r1 r2 = r1 + r2

cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = recExpA (const 0) (const $ const (+)) contarProd1 
                where contarProd1 (Cte 1) _ r1 r2 = 1 + r1 + r2
                      contarProd1 _ (Cte 1) r1 r2 = 1 + r1 + r2
                      contarProd1 _ _ r1 r2 = r1 + r2


