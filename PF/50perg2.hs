module P50 where
import Data.Char
--exemplos
lista = [1,2,3]
lista1 = [10,20,30]

--1
myenumFromTo ::  Int -> Int ->[Int]
myenumFromTo i f
	| i==f = [f] 
	| otherwise = i:myenumFromTo (i+1) f
--2
myenumFromThenTo ::  Int -> Int-> Int -> [Int]
myenumFromThenTo i d f
	| i>f = []
	| i==f = [f]
	| otherwise = i:myenumFromThenTo (i+(d-i)) (d+(d-i)) f
--3
pp ::  [a] -> [a] -> [a]
pp [] [] = []
pp x [] = x
pp [] y = y
pp (x:xs) (y:yx) = x:(pp xs (y:yx))
--4
ee :: [a] -> Int -> a
ee (h:t) n
	| n==0 = h
	| otherwise = ee t (n-1)
--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse x = last(x):myreverse(init(x))
--6
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (h:t)
	| n==1 = [h]
	| otherwise = h:(mytake (n-1) t) 
--7
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (h:t)
	| n==1 = t
	| otherwise = mydrop (n-1) t 
--8
myzip :: [a] -> [b] -> [(a,b)]
myzip [] [] = []
myzip [] y = []
myzip x [] = []
myzip (x:xs) (y:ys) = (x,y):(myzip xs ys)
--9
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem n (h:t) 
	| h==n = True
	| otherwise = myelem n t
--10
myreplicate :: Int -> a ->[a]
myreplicate n l 
	| n==0 = []
	| otherwise = l:(myreplicate (n-1) l) 
--11
myintersperse :: a -> [a] ->[a]
myintersperse n [] = []
myintersperse n (h:[]) = [h]
myintersperse n (h:t) = h:n:myintersperse n t
--12
group ::Eq a => [a] -> [[a]]
group [] = []
group (h:t) = (replicate np h):(group tp) where (np,tp) = group2 h t (1,t)


group2 ::Eq a => a -> [a] ->(Int,[a])-> (Int,[a])
group2 n [] (np,tp) = (np,tp)
group2 n (h:t) (np,tp)
	| n==h = group2 n t (np+1,t)
	| otherwise = (np,tp)


--13
myconcat ::[[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h++(myconcat t)
--14
inits :: [a] -> [[a]]
inits x = reverse(inits2 x)

inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 x = x:(inits2(init x))

--15
tails :: [a] -> [[a]]
tails [] = [[]]
tails x = x:(tails (tail (x)))
--16
isPrefixOf :: Eq a => [a]-> [a] -> Bool
isPrefixOf [] [] = True
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) 
	| x==y = isPrefixOf xs ys
	| otherwise = False
--17
isSuffixOf ::Eq a => [a]-> [a] -> Bool
isSuffixOf [] [] = True
isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf x y 
	| last(x)==last(y) = isSuffixOf (init(x)) (init(y))
	| otherwise = False
--18
isSubsequenceOf :: Eq a =>[a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf _ [] = False
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) (y:ys)
	| x==y = isSubsequenceOf xs ys
	| otherwise = isSubsequenceOf (x:xs) ys
--19
elemIndices :: Eq a => a ->[a] -> [Int]
elemIndices a x = elemIndices2 a (zip x [0..])

elemIndices2 :: Eq a => a ->[(a,Int)] -> [Int]
elemIndices2 a [] = []
elemIndices2 a (x:t) 
	| a == fst (x) = snd(x):elemIndices2 a t
	| otherwise = elemIndices2 a t
--20
nub :: Eq a => [a] -> [a]
nub x = nub2 x []

nub2 :: Eq a => [a] -> [a] -> [a]
nub2 [] l = l
nub2 (x:xs) l = if (elem x l) then nub2 xs l else nub2 xs (l++[x])  
--21
mydelete :: Eq a => a -> [a]-> [a]
mydelete _ [] = []
mydelete a (x:xs) 
	| a==x = xs
	| otherwise = x:mydelete a xs 
--22
dd:: Eq a => [a] -> [a]-> [a]
dd [] _ = []
dd x [] = x 
dd x (h:t) = dd (mydelete h x) t
--23
union :: Eq a => [a] -> [a]-> [a]
union [] _ = []
union x [] = x
union x (h:t) = if elem h x then union x t else union (x++[h]) t
--24
intersect :: Eq a => [a] ->[a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) y = if elem x y then [x]++intersect xs y else intersect xs y  
--25
insert :: Ord a => a -> [a]-> [a]
insert a [] = [a]
insert a (h:t) 
	| a>h = h:insert a t
	| otherwise = a:h:t
--26
myunwords ::[String] -> String
myunwords [] = ""
myunwords (h:[]) = h
myunwords (h:t) = h++" "++(myunwords t)
--27
myunlines :: [String] -> String
myunlines [] = ""
myunlines (h:t) = h++"\n"++(myunwords t)
--28
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = pMaior2 (zip t [1..]) (h,0)

pMaior2 :: Ord a => [(a,Int)] -> (a,Int) -> Int
pMaior2 [] (n,p) = p
pMaior2 ((h,p1):t) (n,pm)
	| h>n = pMaior2 t (h,p1)
	| otherwise =pMaior2 t (n,pm)
--29
temRepetidos ::Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = if elem h t then True else temRepetidos t 
--30
algarismos ::[Char] -> [Char]
algarismos [] = []
algarismos (h:t) 
	| ord(h)>ord('0') && ord(h)<ord('9') = h:algarismos t
	| otherwise = algarismos t
--31
posImpares :: [a] -> [a] 
posImpares a = posImpares2 (zip a [0..])

posImpares2 :: [(a,Int)]->[a]
posImpares2 [] = []
posImpares2 ((h,n):t)
	| mod n 2 == 1 = h:posImpares2 t
	| otherwise = posImpares2 t
--32
posPares :: [a] -> [a]
posPares a = posPares2 (zip a [0..])

posPares2 :: [(a,Int)]->[a]
posPares2 [] = []
posPares2 ((h,n):t)
	| mod n 2 == 0 = h:posPares2 t
	| otherwise = posPares2 t
--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True 
isSorted (h:[]) = True
isSorted (h1:h2:t)
	| h1>h2 = False
	| otherwise = isSorted (h2:t) 
--34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t) 
--35
menor :: String -> String -> Bool
menor [] [] = False
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) 
	| ord(x)<=ord(y) = menor xs ys
	| otherwise = False
--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((h,n):t) 
	| a==h = True
	| otherwise = elemMSet a t
--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,c):t) = c+lengthMSet t
--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((h,n):t) = (replicate n h)++(converteMSet t)
--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet l [] = [(l,1)]
insereMSet l ((l1,n):t)
	| l==l1 = [(l,(n+1))]++t
	| otherwise =[(l1,n)]++insereMSet l t
--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet l [] = []
removeMSet l ((l1,n):t)
	| l==l1 = t 
	| otherwise =[(l1,n)]++removeMSet l t
--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet x = constroiMSet2 x [] 


constroiMSet2 :: Ord a => [a] ->[(a,Int)]-> [(a,Int)]
constroiMSet2 [] x = x
constroiMSet2 (h:t) x= constroiMSet2 t (insereMSet h x) 
--42
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers x = (partitionLeft x,partitionRight x) 
	where partitionRight []=[];
		  partitionRight ((Right b):t) = b:partitionRight t;
		  partitionRight ((Left a):t) = partitionRight t;
		  partitionLeft [] = [];
		  partitionLeft ((Left a):t) = a:partitionLeft t;
		  partitionLeft ((Right b):t) = partitionLeft t;
--43
catMaybes ::[Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes (Just a:t) = a:catMaybes t


data Movimento = Norte | Sul | Este | Oeste deriving Show


--44

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Este:t) = posicao (x+1,y) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t
--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
	| xi==xf && yi==yf = []
	| yi<yf = Norte:caminho (xi,yi+1) (xf,yf)
	| yi>yf = Sul:caminho (xi,yi-1) (xf,yf)
	| xi<xf = Este:caminho (xi+1,yi) (xf,yf)
	| xi>xf = Oeste:caminho (xi-1,yi) (xf,yf)
--46
vertical :: [Movimento] -> Bool
vertical [] = False
vertical (Oeste:t) = False
vertical (Este:t) = False
vertical (_:[]) = True
vertical (_:t) = vertical t
--47
data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral ((Pos a b):t) = maisCentral2 t (a,b)
	where maisCentral2 [] (x,y) = (Pos x y);
		  maisCentral2 ((Pos x y):t) (xm,ym) = if x*x + y*y < xm*xm+ym*ym then maisCentral2 t (x,y) else maisCentral2 t (xm,ym)
--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y) ((Pos x1 y1):t) 
	| abs (x-x1)<=1 && abs (y-y1)<=1 && (x1/=x || y1/=y)= (Pos x1 y1):vizinhos (Pos x y) t
	| otherwise = vizinhos (Pos x y) t

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada (h:[]) = True
mesmaOrdenada ((Pos x y):(Pos x1 y1):t) = x==x1 && mesmaOrdenada ((Pos x1 y1):t) 
--50
data Semaforo = Verde | Amarelo | Vermelho deriving Show
interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (h:[]) = True
interseccaoOK (Vermelho:y:t) =interseccaoOK (y:t)
interseccaoOK (y:Vermelho:t) = interseccaoOK (y:t)
interseccaoOK (x:y:t) = False
