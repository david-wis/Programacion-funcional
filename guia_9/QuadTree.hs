data QuadTree a = LeafQ a 
                    | NodeQ (QuadTree a) (QuadTree a) 
                            (QuadTree a) (QuadTree a) 

data Color = RGB Int Int Int deriving Eq

type Image = QuadTree Color


instance Show Color where
    show (RGB r g b) = "RGB(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

instance Show a => Show (QuadTree a) where
    show = showQuadTree 0
      where
        showQuadTree :: Show a => Int -> QuadTree a -> String
        showQuadTree indent (LeafQ a) = replicate indent '\t' ++ "LeafQ " ++ show a ++ "\n"
        showQuadTree indent (NodeQ a b c d) =
            replicate indent '\t' ++ "NodeQ ->\n" ++
            showQuadTree (indent + 1) a ++
            showQuadTree (indent + 1) b ++
            showQuadTree (indent + 1) c ++
            showQuadTree (indent + 1) d


heightQT :: QuadTree a -> Int
heightQT (LeafQ _) = 0
heightQT (NodeQ t1 t2 t3 t4) = 1 + max (max (max (heightQT t1) (heightQT t2)) (heightQT t3)) (heightQT t4)

countLeavesQT :: QuadTree a -> Int
countLeavesQT (LeafQ _) = 1
countLeavesQT (NodeQ t1 t2 t3 t4) = countLeavesQT t1 + countLeavesQT t2 + countLeavesQT t3 + countLeavesQT t4

sizeQT :: QuadTree a -> Int
sizeQT (LeafQ _) = 1
sizeQT (NodeQ t1 t2 t3 t4) = 1 + sizeQT t1 + sizeQT t2 + sizeQT t3 + sizeQT t4

compress :: (Eq a) => QuadTree a -> QuadTree a
compress (LeafQ e) = LeafQ e
compress (NodeQ t1 t2 t3 t4) = compressNode (compress t1) (compress t2) (compress t3) (compress t4)
                                                        
compressNode :: (Eq a) => QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a
compressNode (LeafQ e1) (LeafQ e2) (LeafQ e3) (LeafQ e4) = if e1 == e2 && e2 == e3 && e3 == e4
                                                                    then LeafQ e1
                                                                    else NodeQ (LeafQ e1) (LeafQ e2) (LeafQ e3) (LeafQ e4)
compressNode t1 t2 t3 t4 = NodeQ t1 t2 t3 t4


uncompress :: QuadTree a -> QuadTree a 
uncompress t = uncompressWithHeight t (heightQT t)


uncompressWithHeight :: QuadTree a -> Int -> QuadTree a
uncompressWithHeight (LeafQ e) 0 = LeafQ e
uncompressWithHeight (LeafQ e) h = let t' = uncompressWithHeight (LeafQ e) (h-1) in NodeQ t' t' t' t'
uncompressWithHeight (NodeQ t1 t2 t3 t4) h = NodeQ (uncompressWithHeight t1 (h-1)) (uncompressWithHeight t2 (h-1)) (uncompressWithHeight t3 (h-1)) (uncompressWithHeight t4 (h-1))

-- Precondicion: n = 4^k >= heightQT t
render :: Image -> Int -> Image 
render img n = uncompressWithHeight img (log4int n)

log4int :: Int -> Int
log4int 1 = 0
log4int n = 1 + log4int (n `div` 4)

createBigImage =
    NodeQ (NodeQ (LeafQ (RGB 255 0 0)) (LeafQ (RGB 255 0 0)) (LeafQ (RGB 255 0 0)) (LeafQ (RGB 255 0 0))) (NodeQ (LeafQ (RGB 255 255 255)) (LeafQ (RGB 128 128 128)) (LeafQ (RGB 64 64 64)) (LeafQ (RGB 0 0 0))) (NodeQ (LeafQ (RGB 255 127 127)) (LeafQ (RGB 127 255 127)) (LeafQ (RGB 127 127 255)) (LeafQ (RGB 255 255 255))) (NodeQ (LeafQ (RGB 0 255 255)) (LeafQ (RGB 255 0 255)) (LeafQ (RGB 255 255 0)) (LeafQ (RGB 0 0 0)))

