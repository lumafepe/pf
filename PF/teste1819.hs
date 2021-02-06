module Teste where
import Data.Char
import System.Random
--1V
elemIndices ::  Eq a => a -> [a] -> [Int]
elemIndices x l = ei x l 0
    where ei:: Eq a => a -> [a] -> Int -> [Int]
          ei x [] n = []
          ei x (h:t) n = if x==h then n:ei x t (n+1) else ei x t (n+1)
isSubsequenceOf ::  Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf h [] = False
isSubsequenceOf (h:t) l = isSubsequenceOf t (iso h l)
    where iso :: Eq a => a -> [a] -> [a]
          iso x [] = []
          iso x (h:t) = if x==h then t else iso x t
--2?
--3V
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t)
    | isDigit h = (h:a,b)
    | otherwise = (a,h:b)
    where (a,b)=digitAlpha t
--4?
--5
type Mat a = [[a]]

getElem ::  Mat a -> IO a
getElem (h:t) = do linha <- randomRIO (0,length((h:t))-1)
                   coluna <- randomRIO (0,length(h)-1)
                   return (((h:t)!!linha)!!coluna)
magic ::  Mat a -> Bool
magic a = a
    where b = map sum a 
          b1 = map sum (transpose a)
          b2 = map sum [(a!!x)!!x|x<-[0..length(a)]]



transpose:: Mat a -> Mat a
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)