module Ficha2 where
import Data.Char 
-- a 39
--b [8,12]
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h * 2 : dobros t)

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t) = if h == c then 1+ numOcorre c t else numOcorre c t 

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h > 0 then positivos t else False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h>0 then h:soPos t else soPos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h<0 then h + somaNeg t else somaNeg t

tresUlt :: [a] -> [a]
tresUlt (h:t) = if length t > 3 then tresUlt t else if length t == 3 then t else if length t <=2 then (h:t) else [] 

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = (b : segundos t)

nosPrimeiros :: (Eq a) =>  a -> [(a,b)] -> Bool
nosPrimeiros n [] = False
nosPrimeiros n ((a,b):t) = if n == a then True else nosPrimeiros n t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ( (a,b,c) : h ) = (a+x1,b+x2,c+x3) where (x1,x2,x3) = (sumTriplos h)

sumTriplos2 :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos2 l = g (0,0,0) l 
g (a,b,c) [] = (a,b,c)
g (a,b,c) ((z1,z2,z3):t) = g (a+z1,b+z2,c+z3) t

--3

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)  | ord(h)>=48 && ord(h)<58 = (h:soDigitos t)
                 | otherwise = soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)   
	| ord(h)<=122 && ord(h)>=97 = 1 + minusculas t
	| otherwise = minusculas t  


nums :: String -> [Int]
nums [] = []
nums (h:t) 
      | ord(h)>=48 && ord(h)<58  = ((ord(h)-48):nums t)
      | otherwise = nums t

--4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta a [] = 0
conta a ((n,e):t) 
	| e==a = 1 + conta a t
	| otherwise = conta a t


grau :: Polinomio -> Int
grau ((a,e):[]) = e
grau ((a1,e1):(a2,e2):t) 
	|  e1>e2 = grau ((a1,e1):t)
	|  otherwise = grau ((a2,e2):t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((a,e):t)
	| e==n = ((a,e):(selgrau n t))
	| otherwise = selgrau n t
{--
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,e):t) = (((a * e),(e - 1)):(deriv t))
--}
calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((a,e):t) = a*(n^e)+ calcula n t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,e):t) 
	| a==0 = simp t
	| otherwise = ((a,e):(simp t))

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a1,e1) ((a2,e2):t) = (((a1 * a2),(e1 + e2)):(mult (a1,e1) t ))

produto ::  Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p = (mult h p) ++ (produto t p)

