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
