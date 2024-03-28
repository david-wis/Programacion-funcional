data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

--a)
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

--b)
ddOfInt :: Int -> DigDec 
ddOfInt n = case n of
    0 -> D0
    1 -> D1
    2 -> D2
    3 -> D3
    4 -> D4
    5 -> D5
    6 -> D6
    7 -> D7
    8 -> D8
    9 -> D9

--c)
nextDD :: DigDec -> DigDec
nextDD d = ddOfInt ((ddAsInt d + 1) `mod` 10)

--d)
prevDD :: DigDec -> DigDec
prevDD d = ddOfInt ((ddAsInt d - 1) `mod` 10)
