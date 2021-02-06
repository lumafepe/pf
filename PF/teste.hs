module Teste where
--1
type MSet a = [(a,Int)]

t1:: MSet Char
t1=[('b',4),('a',2),('c',1)]


cardMSet ::  MSet a -> Int
cardMSet [] = 0
cardMSet ((a,n):t) = n+cardMSet t


moda ::  MSet a -> [a]
moda ((a,n):t) = take b (repeat m)
	where (m,b)= foldr (\x y -> if snd(y)>snd(x) then y else x) (a,n) t 

repeat' :: Int->a->[a]
repeat' 0 a = []
repeat' n a = a:repeat' (n-1) a

converteMSet ::  MSet a -> [a]
converteMSet [] = []
converteMSet ((a,n):t) = (repeat' n a )++x
	where x = converteMSet t

addNcopies ::  Eq a => MSet a -> a -> Int -> MSet a
addNcopies ((a,n):t) an nn 
	| elem an (map (fst) ((a,n):t)) = addNcopies1 ((a,n):t) an nn
	| otherwise = addNcopies2 ((a,n):t) an nn

addNcopies2 [] an nn = (an,nn):[]
addNcopies2 ((a,n):t) an nn
	| nn>n = (an,nn):(a,n):t
	| otherwise = (a,n):addNcopies2 t an nn
	
addNcopies1 x an nn = addNcopies (i++ty) an ((snd tx) + nn )
	where (i,tx:ty) = splitAt (posigual (map (fst) x) an) x



posigual :: Eq a => [a] -> a ->Int
posigual [] _ = 0
posigual (h:t) a 
	| h == a = posigual [] a
	| otherwise = 1+posigual t a
--2
data SReais = AA Double Double 
            | FF Double Double
            | AF Double Double 
            | FA Double Double
            | Uniao SReais SReais

instance Show SReais where
	show (AA x y) = "]"++(show x)++","++(show y)++"["
	show (FF x y) = "["++(show x)++","++(show y)++"]"
	show (AF x y) = "]"++(show x)++","++(show y)++"]"
	show (FA x y) = "["++(show x)++","++(show y)++"["
	show (Uniao x y) = "("++(show x)++" U "++(show y)++")"
pertence ::  Double-> SReais -> Bool
pertence x (AA i f) = x>i && x<f
pertence x (FF i f) = x>=i && x<=f
pertence x (AF i f) = x>i && x<=f
pertence x (FA i f) = x>=i && x<f
pertence x (Uniao a b) = pertence x a || pertence x b

tira ::  Double -> SReais -> SReais
tira x a
	| pertence x a = tira2 x a
	| otherwise = a

tira2 :: Double -> SReais -> SReais
tira2 n (AA i f) = (Uniao (AA i n) (AA n f))
tira2 n (FF i f) 
	| n==i = (AF i f)
	| n==f = (FA i f)
	| otherwise = (Uniao (FA i n) (AF n f))
tira2 n (AF i f) 
	| n==f = (AA i f)
	| otherwise = (Uniao (AA i n) (AF n f))
tira2 n (FA i f)
	| n==i = (AA i f)
	| otherwise = (Uniao (FA i n) (AA n f))
tira2 n (Uniao x y) 
	| pertence n x = (Uniao (tira2 n x) y)
	| otherwise = (Uniao x (tira2 n y)) 
--3
data RTree a = R a [RTree a]

arv=R 5 [ R 4 [ R 3 [R 17 []], R 2 [], R 7 []],
	      R 10 [],
	      R 1 [ R 8 [ R 0 [], R 20 [], R 15 [], R 39 [] ],
	      		R 12 [] ]
	    ]
r1::[Int]->[Int]
r1 [] = []
r1 (h:t) = (h-1):r1 t


percorre ::  [Int] -> RTree a -> Maybe [a]
percorre a x = percorre2 (r1 a) x []

percorre2 :: [Int] -> RTree a -> [a] -> Maybe [a]
percorre2 [] _ ac = Just ac 
percorre2 (h:t) (R a x) ac
	| h<= length x = percorre2 t (x!!h) (ac++[a])
	| otherwise = Nothing

