data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

instance Show DigDec where
    show d = case d of
        D0 -> "0"
        D1 -> "1"
        D2 -> "2"
        D3 -> "3"
        D4 -> "4"
        D5 -> "5"
        D6 -> "6"
        D7 -> "7"
        D8 -> "8"
        D9 -> "9"

type NDec = [DigDec]

ddAsInt :: DigDec -> Int
ddAsInt d = case d of
    D0 -> 0
    D1 -> 1  
    D2 -> 2
    D3 -> 3
    D4 -> 4
    D5 -> 5
    D6 -> 6
    D7 -> 7
    D8 -> 8
    D9 -> 9

nextDD :: DigDec -> DigDec
nextDD D0 = D1
nextDD D1 = D2
nextDD D2 = D3
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0

--d)
prevDD :: DigDec -> DigDec
prevDD D0 = D9
prevDD D1 = D0
prevDD D2 = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8

evalND :: NDec -> Int
evalND [] = 0
evalND (d:ds) = ddAsInt d + 10 * evalND ds

normalizarND :: NDec -> NDec
normalizarND [] = []
normalizarND (d:ds) = quitarCeros d (normalizarND ds)
                    where quitarCeros D0 [] = []
                          quitarCeros d' ds' = d' : ds'

succNDec :: NDec -> NDec
succNDec [] = [D1]
succNDec (d:ds) = case d of 
                    D9 -> D0 : succNDec ds
                    _  -> nextDD d : ds

addNDec :: NDec -> NDec -> NDec
addNDec [] ds2 = ds2
addNDec (d:ds) ds2 = let (d':ds2') = addDDND d ds2 
                     in d' : addNDec ds ds2'
                        

addDDND :: DigDec -> NDec -> NDec
addDDND D0 ds = ds
addDDND D1 ds = succNDec ds
addDDND D2 ds = addDDND D1 (succNDec ds)
addDDND D3 ds = addDDND D2 (succNDec ds)
addDDND D4 ds = addDDND D3 (succNDec ds)
addDDND D5 ds = addDDND D4 (succNDec ds)
addDDND D6 ds = addDDND D5 (succNDec ds)
addDDND D7 ds = addDDND D6 (succNDec ds)
addDDND D8 ds = addDDND D7 (succNDec ds)
addDDND D9 ds = addDDND D8 (succNDec ds)

-- Con fold

addNDec' :: NDec -> NDec -> NDec
addNDec' = foldr f id
         where f d h ds2 = let (d':ds2') = addDDND d ds2
                           in d' : h ds2'