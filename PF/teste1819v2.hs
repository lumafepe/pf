module Teste where
import Data.Char 

elemIndices ::  Eq a => a -> [a] -> [Int]
elemIndices n l = aux n posl
	where posl=(zip [0..] l)
aux::Eq a => a->[(Int,a)]->[Int]
aux n [] = []
aux n ((a,b):t) 
	| n == b = a:aux n t
	| otherwise = aux n t
isSubsequenceOf ::  Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (x:xs)
	| x==h = isSubsequenceOf t xs
	| otherwise = isSubsequenceOf (h:t) xs

data BTree a = Empty | Node a (BTree a) (BTree a)
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) 
	|isDigit(h) = (h:a,b)
	| otherwise = (a,h:b)
	where (a,b)=digitAlpha t


data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
firstSeq ::  Seq a -> a
firstSeq (Cons a l) = a
firstSeq (App x y)
	| temelem x = firstSeq x
	| otherwise = firstSeq y

temelem::Seq a ->Bool
temelem (Nil) = False
temelem (Cons a l) = True
temelem (App x y) = temelem x || temelem y  

instance Show a =>Show Seq a  where	
	show (a)