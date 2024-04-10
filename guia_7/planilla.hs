type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
data Equipo = Becario Nombre 
            | Investigador Nombre Equipo Equipo Equipo

-- show for planilla and equipo
instance Show Planilla where
    show Fin = "Fin"
    show (Registro n p) = n ++ " -> " ++ show p

instance Show Equipo where
    show (Becario n) = "Becario " ++ n
    show (Investigador n e1 e2 e3) = "Investigador " ++ n ++ " (" ++ show e1 ++ ") (" ++ show e2 ++ ") (" ++ show e3 ++ ")"
--

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro _ p) = 1 + largoDePlanilla p

esta :: Nombre -> Planilla -> Bool
esta _ Fin = False
esta nom (Registro nom' p) = (nom == nom') || esta nom p

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas (Registro n p) p2 = Registro n (juntarPlanillas p p2)
juntarPlanillas Fin p2 = p2

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _) = 0
nivelesJerarquicos (Investigador _ e1 e2 e3) = 1 + max (max (nivelesJerarquicos e1) (nivelesJerarquicos e2)) (nivelesJerarquicos e3)

cantidadDeIntegrantes :: Equipo -> Int 
cantidadDeIntegrantes (Becario _) = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

planillaDeIntegrantes :: Equipo -> Planilla 
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n $ juntarPlanillas (juntarPlanillas (planillaDeIntegrantes e1) (planillaDeIntegrantes e2)) (planillaDeIntegrantes e3)
planillaDeIntegrantes (Becario n) = Registro n Fin

planillaDeIntegrantes' :: Equipo -> Planilla 
planillaDeIntegrantes' (Investigador n e1 e2 e3) = Registro n $ foldr1 juntarPlanillas $ planillaDeIntegrantes' <$> [e1, e2, e3]
planillaDeIntegrantes' (Becario n) = Registro n Fin
