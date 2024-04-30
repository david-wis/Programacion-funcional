import TableroR 
import Rowbstones

-- i)
evalExpRInt :: ExpR Int -> TableroR -> Int
evalExpRInt (Lit n) _ = n
evalExpRInt (PuedeMover _) t = boom "PuedeMover es una operacion booleana" t
evalExpRInt NroBolitas t = nroBolitas t
evalExpRInt HayBolitas t = boom "HayBolitas es una operacion booleana" t
evalExpRInt (UnOp uop e) t = evalUnOpInt t uop (evalExpRInt e t)
evalExpRInt (BinOp bop e1 e2) t = evalBinOpInt t bop (evalExpRInt e1 t) (evalExpRInt e2 t)

evalUnOpInt :: TableroR -> UOp -> Int -> Int 
evalUnOpInt t Siguiente = (+1) 
evalUnOpInt t Previo = flip (-) 1
evalUnOpInt t uop = boom "Operacion invalida" t

evalBinOpInt :: TableroR -> BOp -> Int -> Int -> Int 
evalBinOpInt t Mas = (+)
evalBinOpInt t Por = (*) 
evalBinOpInt t _ = boom "Operacion invalida" t

-- ii)
evalExpRBool :: ExpR Bool -> TableroR -> Bool
evalExpRBool (Lit b) _ = b
evalExpRBool (PuedeMover dir) t = puedeMover dir t
evalExpRBool NroBolitas t = boom "NroBolitas es una operacion entera" t
evalExpRBool HayBolitas t = hayBolitas t 
evalExpRBool (UnOp uop e) t = evalUnOpBool t uop (evalExpRBool e t)
evalExpRBool (BinOp bop e1 e2) t = evalBinOpBool t bop (evalExpRBool e1 t) (evalExpRBool e2 t)

evalUnOpBool :: TableroR -> UOp -> Bool -> Bool
evalUnOpBool t No = not
evalUnOpBool t uop = boom "Operacion invalida" t

evalBinOpBool :: TableroR -> BOp -> Bool -> Bool -> Bool 
evalBinOpBool t YTambien = (&&) 
evalBinOpBool t OBien = (||) 
evalBinOpBool t _ = boom "Operacion invalida" t


--- iii)
expRTieneTipoInt :: ExpR Int -> Bool
expRTieneTipoInt (Lit n) = True
expRTieneTipoInt (PuedeMover _) = False
expRTieneTipoInt NroBolitas = True 
expRTieneTipoInt HayBolitas = False
expRTieneTipoInt (UnOp uop e) = unOpTieneTipoInt uop && expRTieneTipoInt e
expRTieneTipoInt (BinOp bop e1 e2) = binOpTieneTipoInt bop && expRTieneTipoInt e1 && expRTieneTipoInt e2

unOpTieneTipoInt :: UOp -> Bool
unOpTieneTipoInt Siguiente = True
unOpTieneTipoInt Previo = True
unOpTieneTipoInt _ = False

binOpTieneTipoInt :: BOp -> Bool
binOpTieneTipoInt Mas = True
binOpTieneTipoInt Por = True
binOpTieneTipoInt _ = False

-- iv)
expRTieneTipoBool :: ExpR Bool -> Bool
expRTieneTipoBool (Lit n) = True
expRTieneTipoBool (PuedeMover _) = True
expRTieneTipoBool NroBolitas = False 
expRTieneTipoBool HayBolitas = True
expRTieneTipoBool (UnOp uop e) = unOpTieneTipoBool uop && expRTieneTipoBool e
expRTieneTipoBool (BinOp bop e1 e2) = binOpTieneTipoBool bop && expRTieneTipoBool e1 && expRTieneTipoBool e2

unOpTieneTipoBool :: UOp -> Bool
unOpTieneTipoBool No = True
unOpTieneTipoBool _ = False

binOpTieneTipoBool :: BOp -> Bool
binOpTieneTipoBool YTambien = True
binOpTieneTipoBool OBien = True
binOpTieneTipoBool _ = False

-- v)
evalR :: ComandoR -> TableroR -> TableroR
evalR (Mover dir) t = mover dir t
evalR Poner t = poner t
evalR Sacar t = sacar t
evalR NoOp t = t
evalR (Repetir e c) t = evalR (repetir2Secuencia (evalExpRInt e t) c) t
evalR (Mientras e c) t = evalR (mientras2Secuencia e c t) t
evalR (Secuencia c1 c2) t = (evalR c2 . evalR c1) t


evalR' :: ComandoR -> TableroR -> TableroR
evalR' (Mover dir) = mover dir
evalR' Poner = poner
evalR' Sacar = sacar
evalR' NoOp = id
evalR' (Repetir e c) = subst ((evalR' . flip repetir2Secuencia c) . evalExpRInt e) id
evalR' (Mientras e c) = subst (evalR' . mientras2Secuencia e c) id
evalR' (Secuencia c1 c2) = evalR' c2 . evalR' c1


repetir2Secuencia :: Int -> ComandoR -> ComandoR
repetir2Secuencia 0 _ = NoOp
repetir2Secuencia n c = Secuencia c (repetir2Secuencia (n-1) c)

mientras2Secuencia :: ExpR Bool -> ComandoR -> TableroR -> ComandoR
mientras2Secuencia e c t = if evalExpRBool e t 
                           then Secuencia c (Mientras e c)
                           else NoOp

subst f g x = f x (g x)

-- vi)
cantSacar :: ComandoR -> Int
cantSacar (Mover dir) = 0
cantSacar Poner = 0 
cantSacar Sacar = 1 
cantSacar NoOp = 0 
cantSacar (Repetir e c) = cantSacar c
cantSacar (Mientras e c) = cantSacar c
cantSacar (Secuencia c1 c2) = cantSacar c1 + cantSacar c2

-- vii)
seqN = repetir2Secuencia

-- viii) 
repeat2Seq :: ComandoR -> ComandoR
repeat2Seq (Mover dir) = Mover dir
repeat2Seq Poner = Poner
repeat2Seq Sacar = Sacar
repeat2Seq NoOp = NoOp
repeat2Seq (Repetir e c) = case e of
                            Lit n -> seqN n (repeat2Seq c)
                            _ -> Repetir e (repeat2Seq c)
repeat2Seq (Mientras e c) = Mientras e (repeat2Seq c) 
repeat2Seq (Secuencia c1 c2) = Secuencia (repeat2Seq c1) (repeat2Seq c2)

