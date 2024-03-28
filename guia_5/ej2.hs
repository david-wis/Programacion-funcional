data DigBin = O | I

--a)
dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

--b) 
dbAsBool :: DigBin -> Bool
dbAsBool O = False
dbAsBool I = True

--c)
dbOfBool :: Bool -> DigBin
dbOfBool False = O
dbOfBool True = I

--d)
negDB :: DigBin -> DigBin
negDB O = I
negDB I = O







