import NExp

cantExtra :: NExp -> Int
cantExtra (Var _) = 0
cantExtra (NCte _) = 0
cantExtra (NBOp bop e1 e2) = if ambosCtes e1 e2 
                             then 1 
                             else cantExtra e1 + cantExtra e2

ambosCtes :: NExp -> NExp -> Bool
ambosCtes (NCte e1) (NCte e2) = True
ambosCtes _ _ = False
