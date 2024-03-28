data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer 
                | Other String 
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b

tryCatch c f h = case c of 
    Ok v -> f v
    Raise e -> h e 

safeDiv :: Int -> Int -> MayFail Int
safeDiv x 0 = Raise DivByZero
safeDiv x y = Ok (div x y)

result = tryCatch (safeDiv (-10) 2) 
         abs
         (\e -> case e of 
            DivByZero -> -1
            _         -> -2
        ) 
