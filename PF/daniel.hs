module Du where
import System.Random
insert ::  Ord a => a -> [a] ->[a]
insert x [] = [x] 
insert x (h:t)
    | x<h = x:h:t
    | otherwise = h:insert x t
catMaybes ::  [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes (Just h:t) = h:catMaybes t

data Exp a = Const a
           | Var   String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)
{--
instance (Num a,Show a)=>Show (Exp a) where
    show(Const a) = show(a)
    show(Var x) = x
    show(Mais a b) = "( "++(show a)++" + "++(show b)++" )"
    show(Mult a b) = "( "++(show a)++" * "++(show b)++" )"
--}
sortOn ::  Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = aux f x (sortOn f xs)
aux :: Ord b =>(a->b) ->a-> [a]->[a]
aux f x [] = [x]
aux f x (m:ms) |f x <= f m = x : (m:ms)
               |f x > f m = m : aux f x ms

amplitude ::  [Int] -> Int
amplitude [] = 0
amplitude (h:t) = amplitude2 t h h

amplitude2 :: [Int]->Int->Int->Int
amplitude2 [] min max = (max-min)
amplitude2 (h:t) min max 
    | h>max = amplitude2 t min h
    | h<min = amplitude2 t h max
    | otherwise = amplitude2 t min max

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

data Imagem = Quadrado Int| Mover (Int,Int) Imagem| Juntar [Imagem] deriving Show
conta ::  Imagem -> Int
conta (Quadrado x) = 1
conta (Mover y x)=conta x
conta (Juntar (l)) = sum(map conta l)

apaga ::  Imagem -> IO Imagem
apaga x = do n<-randomRIO (0,conta x)
             return (fst(apaga2 x n))

apaga2 :: Imagem->Int-> (Imagem,Int)
apaga2 (Quadrado x) 0 = (Juntar [],0)
apaga2 (Quadrado x) n = (Quadrado x ,n-1)
apaga2 (Mover x y) n = (Mover x a ,b)
  where (a,b) = (apaga2 y n)
apaga2 (Juntar []) n=(Juntar [],n)
apaga2 (Juntar (h:t)) n = (Juntar (juntar [a,r]),nr)
  where (a,b) = (apaga2 h n)
        (r,nr)= apaga2 (Juntar t) b

ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4,Mover (4,3) (Quadrado 2)])

juntar :: [Imagem]->[Imagem]
juntar [] = []
juntar ((Juntar x):t) = x++(juntar t)
juntar (h:t) = [h]++(juntar t)