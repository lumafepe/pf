module Ficha2 where

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h):dobros t

numOcorre :: Char -> String -> Int
numOcorre a [] = 0
numOcorre a (h:t)
	| h==a = 1+numOcorre a t
	| otherwise = numOcorre a t

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = (h>0) && positivos t 

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)
	| h>0 = h:soPos t
	| otherwise = soPos t


somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
	| h<0 = h+somaNeg t
	| otherwise = somaNeg t

tresUlt :: [a] -> [a]
tresUlt (h:t)
	| length t < 3 = (h:t)
	| length t == 3 = t
	| otherwise = tresUlt t

segundos :: [(a,b)] -> [b]
segundos ((a,b):t) = b:segundos t

nosPrimeiros :: (Eq a) =>  a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((b,c):t) = a==b || nosPrimeiros a t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+aa,b+bb,c+cc) 
	where (aa,bb,cc) = sumTriplos t


