module Teste where
import Data.Char
import System.Random
--1V
elemIndices ::  Eq a => a -> [a] -> [Int]
elemIndices x l = map (snd) $ filter (\y -> fst y == x ) $ zip l [0..]

isSubsequenceOf ::  Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf h [] = False
isSubsequenceOf (h:t) l = isSubsequenceOf t (iso h l)
    where iso :: Eq a => a -> [a] -> [a]
          iso x [] = []
          iso x (h:t) = if x==h then t else iso x t
--2
data BTree a = Empty | Node a (BTree a) (BTree a)

lookupAP ::  Ord a => a -> BTree (a,b) -> Maybe b
lookupAP n (Node (a,b) e d) 
  | n==a = Just b
  | n<a = lookupAP n e
  | n>a = lookupAP n d
  | otherwise = Nothing

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node x1 e1 d1) = Node (f x x1) (zipWithBT f e e1) (zipWithBT f d d1)
zipWithBT f _ _ = Empty


--3V
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t)
    | isDigit h = (h:a,b)
    | otherwise = (a,h:b)
    where (a,b)=digitAlpha t
--4
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

firstSeq ::  Seq a -> a 
firstSeq (Cons a l) = a
firstSeq (App e d)
  | soNil e = firstSeq d
  | otherwise = firstSeq e 

soNil:: Seq a ->Bool
soNil (Nil) = True
soNil (Cons a e) = False
soNil (App e d) = soNil e && soNil d

dropSeq ::  Int -> Seq a -> Seq a
dropSeq n (App e d)
  | contaSeq e == n = d
  | contaSeq e < n = dropSeq ( n-contaSeq e ) d
  | otherwise = App (dropSeq n e) d
dropSeq n (Cons a r)
  | contaSeq r < n = (Cons a (dropSeq n r))
  | contaSeq r == n = Nil

contaSeq :: Seq a -> Int 
contaSeq Nil = 0
contaSeq (Cons a e)= 1+ contaSeq e
contaSeq (App e d) = contaSeq e + contaSeq d

instance Show a =>(Show (Seq a)) where
  show a = "<<" ++ (init $ shows2 a) ++ ">>"




shows2 :: Show a => Seq a -> String
shows2 (App e d) = shows2 e ++ shows2 d
shows2 (Cons a e)= show a ++"," ++ shows2 e
shows2 (Nil) =  ""

b :: Mat Int
b = [[6,7,2],[1,5,9],[8,3,4]]
type Mat a = [[a]]


getElem ::  Mat a -> IO a
getElem a = do x <- randomRIO (0,(length a -1))
               y <- randomRIO (0,(length $ head a )-1)
               return ((a!!x)!!y)

magic ::  Mat Int -> Bool
magic l = igual $ (diagonal1:diagonal2:coluna++linha)
  where linha = (map (sum) l )
        coluna = (map (sum) (transpose l))
        diagonal1 = (sum $ map (\x -> (l!!x)!!x) [0..(length l -1)])
        diagonal2 = (sum $ map (\x -> (l!!(x))!!(length l -x-1)) [0..(length l -1)])

igual :: [Int]->Bool
igual (h1:[])=True
igual (h1:h2:t) = h1==h2 && igual (h1:t)

transpose:: Mat a -> Mat a
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)