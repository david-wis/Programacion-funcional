type Name = String
type Content = String
type Path = [Name]
data FileSystem = File Name Content
                | Folder Name [FileSystem] deriving Show


foldFS :: (Name -> Content -> b) -> (Name -> c -> b) -> ([b] -> c) -> FileSystem -> b
foldFS f _ _ (File n c) = f n c
foldFS f g k (Folder n fs) = g n (k (map (foldFS f g k) fs))

-- recFS ???

amountOfFiles :: FileSystem -> Int
amountOfFiles = foldFS (\_ _ -> 1) (\_ qty -> qty) sum


find :: Name -> FileSystem -> Maybe Content
find name = foldFS (\n c -> if n == name then Just c else Nothing) (const (foldr getAnyContent Nothing)) id
          where getAnyContent Nothing m = m
                getAnyContent mx _ = mx

pathOf :: Name -> FileSystem -> Path
pathOf name = foldFS (\n _ -> if n == name then [name] else []) g (firstOrNothing . filter (not . null))
            where g n mp  = if n == name then [name] 
                                         else case mp of 
                                              Nothing -> []
                                              Just p -> n:p

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x


mapContents :: (Content -> Content) -> FileSystem -> FileSystem
mapContents f = foldFS (\n c -> File n (f c)) Folder id

targetedMapContents :: [(Name, Content -> Content)] -> FileSystem -> FileSystem
targetedMapContents ts = foldFS (\n c -> File n (applyFunction (filter ((==) n . fst) ts) c)) Folder id
                       where applyFunction [] = id
                             applyFunction ((_, c):_) = c

-- tests
fileList = File "list.hs" "foldrrrrrrrrrrr"
filePizza = File "pizza.hs" "agregar capas"
fileTree = File "tree.hs" "foldT f g h i j k l m n"
fileMapas = File "mapas.hs" "mapMapa"
folder10 = Folder "Guia 10" [fileList, filePizza]
folder11 = Folder "Guia 11" [fileTree, fileMapas]
folderPF = Folder "PF" [folder10, folder11]
folderEmpty = Folder "Empty" []
fileMongo = File "mongo.js" "mondongo"
folderBD = Folder "BD II" [fileMongo]
folderBackUp = Folder "BackUp" [fileMongo, fileList, File "basura" "     "]
rootDir = Folder "Root" [folderPF, folderEmpty, folderBD, folderBackUp]