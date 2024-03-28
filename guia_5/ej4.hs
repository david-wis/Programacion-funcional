data Medida = Mm Float | Cm Float | Inch Float | Foot Float

asMM :: Medida -> Medida
asMM x = case x of 
    Mm m -> Mm m
    Cm m -> Mm (m*10)
    Inch m -> Mm (m*25.4)
    Foot m -> Mm (m*304.8)

asCm :: Medida -> Medida
asCm x = case x of 
    Mm m -> Cm (m*0.1) 
    Cm m -> Cm m 
    Inch m -> Cm (m*2.54)
    Foot m -> Cm (m*30.48)

asInch :: Medida -> Medida
asInch x = case x of 
    Mm m -> Inch (m*0.039) 
    Cm m -> Inch (m*0.394)
    Inch m -> Inch m 
    Foot m -> Inch (m*12) 

asFoot :: Medida -> Medida
asFoot x = case x of 
    Mm m -> Foot (m*0.003) 
    Cm m -> Foot (m*0.033)
    Inch m -> Foot (m*0.083)
    Foot m -> Foot m 
