(\\)::Eq a =>[a]->[a]->[a]
(\\) l [] = l
(\\) l (h:t) = (\\) (remove2 h l) t

remove2:: Eq a =>a ->[a]->[a]
remove2 a [] = []
remove2 a (h:t)
    | a ==h = t
    | otherwise = h:remove2 a t

type MSet a = [(a,Int)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a l = filter (\x -> fst x /= a) l

calcula :: Eq a => MSet a -> ([a],Int)
calcula l = foldr (\x ac -> if elem (fst x) (fst ac) then ((fst ac),snd ac +snd x) else (fst ac ++ [(fst x)] , snd ac + snd x)) ([],0) l

partes :: String -> Char -> [String]
partes s a = partes2 s a []
partes2::String->Char->String->[String]
partes2 [] _ s = [s]
partes2 (h:t) a s 
    | h ==a = s:partes2 t a []
    | otherwise = partes2 t a (s++[h])


data BTree a = Empty | Node a (BTree a) (BTree a)

remove :: Ord a => a ->BTree a ->BTree a
remove a (Node x e d)
    | x==a = Empty
    | x>a = Node x (remove a e) d
    | otherwise = Node x e (remove a d)
remove a Empty = Empty

instance Show a => (Show (BTree a)) where
    show Empty = "*"
    show (Node x e d) = "("++show e ++" <-"++show x ++"-> "++show d ++")"   

a1= Node 5 (Node 3 Empty Empty) (Node 7 Empty (Node 9 Empty Empty))


sortOn ::Ord b => (a -> b)->[a]->[a]
sortOn f l = map (\x->l!!x) ordenada
    where ordenada = map snd (isort $ zip (map f l) [0..])

isort::Ord b=>[(b,Int)]->[(b,Int)]
isort [] = []
isort (h:t) = insert h (isort t)

insert::Ord b=>(b,Int)->[(b,Int)]->[(b,Int)]
insert (a,b) [] = [(a,b)]
insert (a,b) ((c,d):t) 
    | a<c = (a,b):(c,d):t
    | otherwise = (c,d):insert (a,b) t 

data FileSystem = File Nome | Dir Nome [FileSystem] deriving Show
type Nome = String

fs1 = Dir "usr" [ Dir "xxx" [File "abc.txt",File "readme",Dir "PF" [File "exemplo.hs"]],
                  Dir "yyy" [],Dir "zzz" [Dir "tmp" [],File "teste.c"]]

fichs::FileSystem -> [Nome]
fichs (File nome) = [nome]
fichs (Dir a []) = []
fichs (Dir a l) = foldr (++) [] $ map (fichs) l


dirFiles :: FileSystem->[Nome]->Maybe [Nome]
dirFiles (File a) _ = Nothing
dirFiles (Dir a l) [] = Just (concat $ map fichs l)
dirFiles (Dir a l) (h:t)
    | a==h = catMaybes $ map (\x -> dirFiles x t) l
    | otherwise = Nothing


catMaybes :: [Maybe [Nome]] -> Maybe [Nome]
catMaybes [] = Nothing
catMaybes ((Just x):t) = Just x
catMaybes ((Nothing):t) = catMaybes t

listaFich :: FileSystem -> IO()
listaFich l = do y<-getLine
                 let d=partes y '/'
                     c=dirFiles l d
                     a= if c==Nothing then "Não é uma directoria" else (show c)
                 putStrLn a