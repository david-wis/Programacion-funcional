import Memoria
import NExp

-- Ej 1)

-- i)

evalNExp :: NExp -> Memoria -> Int
evalNExp (Var v) m = case cuantoVale v m of
                       Nothing -> error ("Variable no inicializada " ++ v)
                       Just n -> n
evalNExp (NCte n) _ = n
evalNExp (NBOp op e1 e2) m = evalNBOp op (evalNExp e1 m) (evalNExp e2 m)

evalNBOp :: NBinOp -> Int -> Int -> Int
evalNBOp Add = (+)
evalNBOp Sub = (-)
evalNBOp Mul = (*)
evalNBOp Div = div
evalNBOp Mod = mod 
evalNBOp Pow = (^)


-- ii)
cfNExp :: NExp -> NExp
cfNExp (Var v) = Var v
cfNExp (NCte n) = NCte n
cfNExp (NBOp op e1 e2) = cfNBop op (cfNExp e1) (cfNExp e2)

cfNBop :: NBinOp -> NExp -> NExp -> NExp 
cfNBop op (NCte n1) (NCte n2) = NCte (evalNBOp op n1 n2)
cfNBop op e1 e2 = NBOp op e1 e2

-- exp4mas5 = NBOp Add (NCte 4) (NCte 5)
-- expPor8 = NBOp Mul (NCte 8) exp4mas5 
-- expConVar = NBOp Add (Var "x") expPor8


-- Ej 2)

-- i)

evalBExp :: BExp -> Memoria -> Bool
evalBExp (BCte b) m = b
evalBExp (Not e) m = not (evalBExp e m)
evalBExp (And e1 e2) m = evalBExp e1 m && evalBExp e2 m
evalBExp (Or e1 e2) m = evalBExp e1 m || evalBExp e2 m
evalBExp (ROp rel ne1 ne2) m = evalRelOp rel (evalNExp ne1 m) (evalNExp ne2 m)

evalRelOp :: RelOp -> Int -> Int -> Bool 
evalRelOp Eq = (==)
evalRelOp NEq = (/=)
evalRelOp Gt = (>)
evalRelOp GEq = (>=)
evalRelOp Lt = (<)
evalRelOp LEq = (<=)


-- ii)
cfBExp :: BExp -> BExp
cfBExp (BCte b) = BCte b
cfBExp (Not e) = cfBNot (cfBExp e) 
cfBExp (And e1 e2) = cfBAnd e1 e2
cfBExp (Or e1 e2) = cfBOr e1 e2 
cfBExp (ROp rel ne1 ne2) = cfBROp rel (cfNExp ne1) (cfNExp ne2)

cfBNot :: BExp -> BExp
cfBNot (BCte b) = BCte (not b)
cfBNot e = Not e

cfBAnd :: BExp -> BExp -> BExp
cfBAnd (BCte True) e2 = e2
cfBAnd e1 (BCte True)  = e1 
cfBAnd (BCte False) e2 = BCte False
-- cfBAnd e1 (BCte False)  = BCte False
cfBAnd b1 b2  = And b1 b2

cfBOr :: BExp -> BExp -> BExp
cfBOr (BCte True) e2 = BCte True
-- cfBOr e1 (BCte True)  = BCte True
cfBOr (BCte False) e2 = e2
cfBOr e1 (BCte False)  = e1 
cfBOr b1 b2  = Or b1 b2

cfBROp :: RelOp -> NExp -> NExp -> BExp
cfBROp op (NCte n1) (NCte n2) = BCte (evalRelOp op n1 n2)
cfBROp op e1 e2 = ROp op e1 e2


-- Ej 3)

-- i, ii, iii)
evalProg :: Programa -> Memoria -> Memoria
evalProg (Prog b) = evalBloque b

evalBloque :: Bloque -> Memoria -> Memoria
evalBloque [] m = m
evalBloque (c:cs) m = evalBloque cs (evalComando c m)

evalComando :: Comando -> Memoria -> Memoria
evalComando (Assign n e) m = recordar n (evalNExp e m) m
evalComando (If be b1 b2) m = if evalBExp be m then evalBloque b1 m else evalBloque b2 m
-- evalComando (While be b1) m = if evalBExp be m then evalComando (While be b1) (evalBloque b1 m) else m
evalComando (While be b1) m = evalComando (If be (b1 ++ [While be b1]) []) m

-- factP = (Prog [ Assign "x" (NCte 1)
--           , Assign "y" (NCte 1)
--           , While (ROp LEq (Var "x") (NCte 5))
--               [ Assign "y" (NBOp Mul (Var "y") (Var "x"))
--               , Assign "x" (NBOp Add (Var "x") (NCte 1))
--               ]
--           ])

-- fibP = (Prog [ Assign "x" (NCte 0)
--           , Assign "y" (NCte 1)
--           , Assign "z" (NCte 0)
--           , While (ROp LEq (Var "z") (NCte 10))
--               [ Assign "z" (NBOp Add (Var "x") (Var "y"))
--               , Assign "x" (Var "y")
--               , Assign "y" (Var "z")
--               ]
--           ])

optimizeCF :: Programa -> Programa
optimizeCF (Prog b) = Prog (optimizeBlockCF b)

optimizeBlockCF :: Bloque -> Bloque
optimizeBlockCF [] = []
optimizeBlockCF (c:cs) = cfBlock c ++ optimizeBlockCF cs

cfBlock :: Comando -> Bloque
cfBlock (Assign n e)  = [Assign n (cfNExp e)]
-- cfBlock (If be [] [])  = [] 
cfBlock (If be b1 b2)  = case cfBExp be of
                            BCte True -> optimizeBlockCF b1
                            BCte False -> optimizeBlockCF b2
                            _ -> [If be (optimizeBlockCF b1) (optimizeBlockCF b2)]
-- cfBlock (While be [])  = [] 
cfBlock (While be b1)  = case cfBExp be of
                            BCte False -> []
                            _ -> [While be (optimizeBlockCF b1)]

reducibleProgram = Prog [
    Assign "x" (NCte 1),
    While (BCte True) [
        Assign "x" (NBOp Add (Var "x") (NCte 1)),
        If (BCte True) [
        Assign "x" (NBOp Add (Var "x") (NCte 1))
        ] [
        Assign "x" (NBOp Add (Var "x") (NCte 1))
        ]
    ],
    While (BCte False) [
        Assign "x" (NBOp Add (Var "x") (NCte 1))
    ],
    If (BCte True) [
    Assign "x" (NBOp Add (Var "x") (NCte 1)),
    If (ROp Eq (Var "a") (NCte 4)) [] [],
    If (ROp Eq (Var "a") (NCte 4)) [
        Assign "a" (NCte 5)
    ] []
    ] []]

-- [x = 1,
-- while True do [x = (x+1),
-- if True then [x = (x+1)] else [x = (x+1)]],
-- while False do [x = (x+1)],
-- if False then [x = (x+1),
-- if (a==4) then [] else [],
-- if (a==4) then [a = 4] else []] else []]