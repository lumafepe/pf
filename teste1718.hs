import System.Random
insert :: Ord a => a -> [a]->[a]
insert a [] = [a]
insert a (h:t)
    | h>a = a:h:t
    | otherwise = h:insert a t


catMaybes ::  [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing):t) = catMaybes t
catMaybes ((Just a):t) = a:catMaybes t


data Exp a = Const a
             | Var   String
             | Mais (Exp a) (Exp a)
             | Mult (Exp a) (Exp a)
instance Show a => (Show (Exp a)) where
    show (Const a) = show a
    show (Var s) = s
    show (Mais a b) = "("++show a ++" + "++show b ++")"
    show (Mult a b) = "("++show a ++" * "++show b ++")"



sortOn ::  Ord b => (a -> b) -> [a] -> [a]
sortOn f a = map (\x -> (a!!x) ) lo
    where lo = sort2 $ zip (map f a) [0..] 

sort2 :: Ord a => [(a,Int)]->[Int]
sort2 a = map (snd) $ foldr (\x ac -> insert2 x ac) [] a

insert2 ::Ord a =>(a,Int)->[(a,Int)]->[(a,Int)]
insert2 (a,pos) [] = [(a,pos)]
insert2 (a,pos) ((a1,pos1):t)
    | a < a1 = (a,pos):(a1,pos1):t
    | otherwise = (a1,pos1):(insert2 (a,pos) t)


amplitude ::  [Int] -> Int
amplitude [] = 0
amplitude (h:t) = amplitude2 t h h
amplitude2::[Int]->Int->Int->Int
amplitude2 [] max min = max-min
amplitude2 (h:t) max min 
    | h>max = amplitude2 t h min
    | h<min = amplitude2 t max h
    | otherwise = amplitude2 t max min

sort :: Ord a => [a]->[a]
sort [] = []
sort (h:t) = insert h (sort t)

parte ::  [Int] -> ([Int],[Int])
parte a = parte2 $ sort a

parte2 ::  [Int] -> ([Int],[Int])
parte2 [] = ([],[])
parte2 (h1:h2:h3:t)
    | h2-h1+last t - h3 <h3-h1+amplitude(t) = ([h1,h2],(h3:t))
    | otherwise = parte2 (h1:h3:t)

data Imagem = Quadrado Int
              | Mover (Int,Int) Imagem
              | Juntar [Imagem]
              deriving Show
ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5)
                        ,Quadrado 4,
                         Mover (4,3) (Quadrado 2)])

conta ::  Imagem -> Int
conta (Mover coords i) = conta i
conta (Quadrado i) = 1
conta (Juntar l)= foldr (\x ac -> ac+conta x) 0 l


apaga ::  Imagem -> IO Imagem
apaga imagem = do n <- randomRIO (0,(conta imagem))
                  return $ apagaaux imagem n
                  


apagaaux:: Imagem -> Int -> Imagem
apagaaux (Quadrado x) 0 = Juntar []
apagaaux (Quadrado x) n = Quadrado x
apagaaux (Mover coords x) n = (Mover coords (apagaaux x n))
apagaaux (Juntar l) n 
    | (sum $ map (conta) l) > n = Juntar (apagai l n)
    | otherwise = Juntar l 

apagai::[Imagem]->Int->[Imagem]
apagai (h:t) n
    | conta h < n = h:apagai t (n-conta h)
    | otherwise = (apagaaux h n):t




type Mat a = [[a]]

triSup :: Num a => Mat a -> Bool
triSup h = triSup2 h 0 0

triSup2 :: Num a => Mat a -> Int -> Int->Bool
triSup2 [] _ _ = True
triSup2 (h:t) n 0 = triSup2 t (n+1) (n+1)
triSup2 ((x:xs):t) n n1 = x==0 && triSup2 ((xs):t) n (n1-1)

b :: Mat Int
b = [[6,7,2],[1,5,9],[8,3,4]]