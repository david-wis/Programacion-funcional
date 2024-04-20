-- import EA

data AppList a = Single a
               | Append (AppList a) (AppList a) deriving Show


lenAL :: AppList a -> Int
lenAL (Single _) = 1
lenAL (Append a1 a2) = lenAL a1 + lenAL a2

consAL :: a -> AppList a -> AppList a
consAL e (Single e') = Append (Single e) (Single e')
consAL e (Append a1 a2) = Append (consAL e a1) a2

headAL :: AppList a -> a
headAL (Single e') = e'
headAL (Append a1 a2) = headAL a1

tailAL :: AppList a -> AppList a
tailAL (Single _) = error "Imposible"
tailAL (Append (Single _) a2) = a2
tailAL (Append a1 a2) = Append (tailAL a1) a2

snocAL :: a -> AppList a -> AppList a
snocAL e (Single e') = Append (Single e') (Single e)
snocAL e (Append a1 a2) = Append a1 (consAL e a2)

lastAL :: AppList a -> a
lastAL (Single e') = e'
lastAL (Append a1 a2) = lastAL a2

initAL :: AppList a -> AppList a
initAL (Single _) = error "Imposible"
initAL (Append a1 (Single _)) = a1
initAL (Append a1 a2) = Append a1 (initAL a2)

reverseAL :: AppList a -> AppList a
reverseAL (Single e) = Single e
reverseAL (Append a1 a2) = Append a2 a1

elemAL :: (Eq a) => a -> AppList a -> Bool
elemAL e (Single e') = e == e'
elemAL e (Append a1 a2) = elemAL e a1 || elemAL e a2

appendAL :: AppList a -> AppList a -> AppList a
appendAL (Single e) a' = Append (Single e) a'
appendAL (Append a1 a2) a' = Append a1 (appendAL a' a2)

appListToList :: AppList a -> [a]
appListToList (Single e) = [e]
appListToList (Append a1 a2) = appListToList a1 ++ appListToList a2






