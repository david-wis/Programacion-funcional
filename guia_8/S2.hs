module S2 where

-- Ej 1)

data N = Z | S N

instance Show N where
    show n = show (evalN n)


evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z n = n
addN (S n) n' = S (addN n n')

prodN :: N -> N -> N
prodN Z _ = Z
prodN (S n) n' = addN n' (prodN n n') 

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

----------------------------------------------

-- Ej 2)
type NU = [()]

evalNU :: NU -> Int
evalNU [] = 0
evalNU (_:xs) = 1 + evalNU xs

succNU :: NU -> NU
succNU [] = ():[]
succNU (x:xs) = x : (succNU xs)

addNU :: NU -> NU -> NU
addNU [] n2 = n2
addNU (x:xs) n2 = x : (addNU xs n2)

nu2n :: NU -> N
nu2n [] = Z 
nu2n (_:xs) = S (nu2n xs)

n2nu :: N -> NU
n2nu Z = []
n2nu (S n) = () : n2nu n


----------------------------------------------

-- Ej 3)
data DigBin = O | I deriving (Eq, Show)
type NBin = [DigBin]

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

negDB :: DigBin -> DigBin
negDB O = I
negDB I = O


-- i)
evalNB :: NBin -> Int
evalNB [] = 0
evalNB (x:xs) = dbAsInt x + 2*evalNB xs

-- ii)
normalizarNB :: NBin -> NBin
normalizarNB [] = []
normalizarNB (x:xs) = eliminarCerosADerecha x (normalizarNB xs)
                
eliminarCerosADerecha :: DigBin -> NBin -> NBin
eliminarCerosADerecha O [] = [] 
eliminarCerosADerecha d xs = d : xs

-- iii) Suponiendo un argumento normalizado
-- De derecha a izquierda :P
-- succNB :: NBin -> NBin
-- succNB [] = I:[]
-- succNB (x:xs) = if null xs 
--                 then negDB x : []
--                 else propagarCambio x (succNB xs) xs

-- propagarCambio :: DigBin -> NBin -> NBin -> NBin 
-- propagarCambio x xs ys = if head xs /= head ys && head xs == O
--                          then negDB x : xs 
--                          else x : xs

-- De izquierda a derecha (RP)
succNB :: NBin -> NBin
succNB [] = [I]
succNB (x:xs) = finishSucc'n x xs (succNB xs)


finishSucc'n :: DigBin -> NBin -> NBin  -> NBin
finishSucc'n O xs _ = I : xs 
finishSucc'n I _ xs' = O : xs'

-- iv) 
addNB :: NBin -> NBin -> NBin
addNB [] ys = ys 
addNB xs ys = succNB (addNB (decNB xs) ys)

decNB :: NBin -> NBin
decNB [] = error "Kaboom"
decNB [I] = []
decNB (O:xs) = I : decNB xs
decNB (_:xs) = O : xs

-- Version posta:
addNB' :: NBin -> NBin -> NBin
addNB' xs ys = addWithCarryNB xs ys O

addWithCarryNB :: NBin -> NBin -> DigBin -> NBin
addWithCarryNB [] [] O = [] 
addWithCarryNB [] [] I = [I] 
addWithCarryNB [] (y:ys) c = let (c', d) = addDigBinWithCarry c O y in d : addWithCarryNB [] ys c'
addWithCarryNB (x:xs) [] c = let (c', d) = addDigBinWithCarry c x O in d : addWithCarryNB xs [] c'
addWithCarryNB (x:xs) (y:ys) c = let (c', d) = addDigBinWithCarry c x y in d : addWithCarryNB xs ys c'

addDigBinWithCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addDigBinWithCarry O O O = (O, O)
addDigBinWithCarry O O I = (O, I)
addDigBinWithCarry O I O = (O, I)
addDigBinWithCarry O I I = (I, O)
addDigBinWithCarry I O O = (O, I)
addDigBinWithCarry I O I = (I, O)
addDigBinWithCarry I I O = (I, O)
addDigBinWithCarry I I I = (I, I)

-- v)
nb2n :: NBin -> N
nb2n [] = Z
nb2n xs = S (nb2n (decNB xs))

nb2n' :: NBin -> N
nb2n' [] = Z
nb2n' xs = nb2nPot xs 1 1

nb2nPot :: NBin -> Int -> Int -> N
nb2nPot [] _ _ = Z
nb2nPot (O:xs) _ p' = nb2nPot xs (2*p') (2*p')
nb2nPot (I:xs) 1 p' = S(nb2nPot xs (2*p') (2*p'))
nb2nPot xs p p' = S(nb2nPot xs (p-1) p')


-- vi)
n2bn :: N -> NBin 
n2bn Z = []
n2bn (S n) = succNB (n2bn n)