module Perguntas where
import Data.Char 
--1
enumFromTo2 :: Int -> Int -> [Int]
enumFromTo2 x y
	| x>y = []
	| otherwise = [x] ++ enumFromTo2 (x+1) y 
--2
enumFromThenTo2 :: Int -> Int -> Int -> [Int]
enumFromThenTo2 x y z 
	| x>z = []
	| otherwise = [x] ++ enumFromThenTo2 (x+(y-x)) (y+(y-x)) z
--3
pp :: [a] -> [a] -> [a]
pp [] [] = []
pp [] (h2:t2) = (h2:pp [] t2)
pp (h1:[]) a2 = (h1:pp [] a2)
pp (h1:t1) a2 = (h1:pp t1 a2)
--4
pos :: [a] -> Int -> a
pos (h:t) p
 | p == 0 = h
 | otherwise = pos (t) (p-1)  
--5
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 x = (last(x):(reverse2(init x)))
--6
take2 :: Int -> [a] -> [a]
take2 n [] = []
take2 n (h:t)
	| n==0 = []
	| otherwise = (h:(take2 (n-1) t))
--7
drop2 :: Int -> [a] -> [a]
drop2 n [] = []
drop2 n (h:t)
	| n>0 = drop2 (n-1) t
	| otherwise = (h:(drop2 (n) t))
--8
zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] [] = []
zip2 (h1:t1) (h2:t2) = [(h1,h2)] ++ (zip2 t1 t2)
--9
elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 n (h:t)
	| n==h = True
	| otherwise = elem2 n t 
--10
replicate2 :: Int -> a -> [a]
replicate2 n v
	| n==0 = []
	| otherwise = (v):(replicate2 (n-1) v)
--11
intersperse2 :: a -> [a] -> [a]
intersperse2 n [] = []
intersperse2 n (h:[]) = h:(intersperse2 n [])
intersperse2 n (h:t) = h:n:(intersperse2 n t)
--12
group :: Eq a => [a] -> [[a]]
group [] = []
group (h1:[]) = ([h1]:[])
group (h1:h2:t) = if h1==h2 
	then (group3 n h1):(group (drop n (h1:h2:t))) 
	else [h1]:group (h2:t)
	where n = group2 0 h1 (h1:h2:t)

group2 :: Eq a => Int -> a -> [a] -> Int
group2 n h1 [] = n
group2 n h1 (h:t)
	| h1==h = group2 (n+1) h1 t
	| otherwise = n

group3 :: Eq a => Int -> a -> [a]
group3 0 h = []
group3 n h = h:group3 (n-1) h 
--13
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (h:t) = h++concat2 t
--14
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]
--15
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = [l] ++ tails (tail l)                   --wtf is going on
--16
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] (h2:t2) = True
isPrefixOf (h1:t1) (h2:t2) 
	| h1==h2 = isPrefixOf t1 t2
	| otherwise = False
--17
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] [] = True
isSuffixOf [] (h2:t2) = False
isSuffixOf (h1:t1) [] = False
isSuffixOf (h1:t1) (h2:t2) 
	| h1==h2 = isSuffixOf t1 t2
	| otherwise = isSuffixOf (h1:t1) t2 --n funciona se [10,30] [10,20,30]
--18
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h1:t1) (h2:t2)
	| h1==h2 = isSubsequenceOf t1 t2
	| otherwise = isSubsequenceOf (h1:t1) t2 


--19
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices n (h:t)
    | n == h =  0: map (+1) (elemIndices n t)
    | otherwise = map (+1) (elemIndices n t)


--20
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) = if nub2 h t then nub t else h:nub t

nub2 :: Eq a => a -> [a] -> Bool
nub2 n [] = False
nub2 n (h:t) 
	| n==h = True
	| otherwise = nub2 n t

--21
delete2 :: Eq a => a -> [a] -> [a]
delete2 _ [] = []
delete2 n (h:t)
	| n==h = delete2 n t
	| otherwise = h:(delete2 n t) 

--22
dd :: Eq a => [a] -> [a]-> [a]
dd [] x = []
dd x [] = x
dd x (h:t) = dd (dd2 h x) t  

dd2 :: Eq a => a -> [a]-> [a]
dd2 n [] = []
dd2 n (h:t)
	| n==h = t
	| otherwise = h:dd2 n t
--23

union :: Eq a => [a] -> [a] -> [a]
union x [] = x
union x (h:t) = if unionv x h then union x t else (union x t ++[h])


unionv :: Eq a=> [a] -> a ->Bool
unionv [] n = False
unionv (h:t) n 
	| h==n = True
	| otherwise = unionv t n

--24
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] x = []
intersect (h1:t1) x 
	| (intersect2 h1 x) = h1:(intersect t1 x)
	| otherwise = intersect t1 x

intersect2 :: Eq a => a -> [a] -> Bool
intersect2 n [] = False
intersect2 n (h:t) 	
	| n== h = True
	| otherwise = intersect2 n t
--25
insert :: Ord a => a -> [a] -> [a]
insert n [] = (n:[])
insert n (h:t)
	| h>n = n:h:t
	| otherwise = h:insert n t   
--28
pMaior :: Ord a => [a] -> Int
pMaior x = pMaior2 x 0 

pMaior2 :: Ord a => [a] -> Int -> Int
pMaior2 (h1:[]) n = n
pMaior2 (h1:h2:t) n 
	| h1>h2 = pMaior2 (h1:t) n
	| otherwise = pMaior2 (h2:t) (n+1)
--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = if (temRepetidos2 h t) then True
									 		else temRepetidos t
temRepetidos2 :: Eq a => a -> [a] -> Bool
temRepetidos2 n [] = False
temRepetidos2 n (h:t) 
	| n == h = True
	| otherwise = temRepetidos2 n t 
--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
	| ord(h)>=48 && ord(h)<=58 = h:(algarismos t)
	| otherwise = algarismos t
--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (h1:[]) = []
posImpares (h1:h2:t) = h2:(posImpares t)
--32
posPares :: [a] -> [a]
posPares [] = []
posPares (h1:[]) = h1:posPares []
posPares (h1:h2:t) = h1:(posPares t)
--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True 
isSorted (h1:[]) = True
isSorted (h1:h2:t) 	
	| h1<=h2 = isSorted (h2:t)
	| otherwise = False
--34

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = if (h:t)==n then h:iSort t else iSort n where n = insert h t 

--35
menor :: String -> String -> Bool
menor _ [] = False
menor [] _ = True
menor (h1:t1) (h2:t2) 
	| ord(h1)>ord(h2) = False
	| otherwise = menor t1 t2
--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet n [] = False
elemMSet n ((c,d):t) 
	| n == c = True
	| otherwise = elemMSet n t
--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):t) = b+lengthMSet t
--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,0):t) = converteMSet t
converteMSet ((a,b):t) = a:converteMSet ((a,(b-1)):t)
--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet l [] = (l,1):[]
insereMSet l ((a,b):t)
	| l == a = ((a,b+1):t)
	| otherwise = (a,b):insereMSet l t
--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet l [] = []
removeMSet l ((a,b):t) 
	| l==a = t
	| otherwise = (a,b):removeMSet l t
--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)
--42







--43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = [maybe h ]++[catMaybes t]


data Movimento = Norte | Sul | Este | Oeste deriving Show
--44
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao	(x,y) ((Norte):t) = posicao (x,y+1) t 
posicao	(x,y) ((Sul):t) = posicao (x,y-1) t
posicao	(x,y) ((Oeste):t) = posicao (x-1,y) t 
posicao	(x,y) ((Este):t) = posicao (x+1,y) t
--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) 
	| xi==xf && yi==yf = []
	| xi<xf = Este:caminho (xi+1,yi) (xf,yf)
	| xi>xf = Oeste:caminho (xi+1,yi) (xf,yf)
	| yi>yf = Sul:caminho (xi,yi-1) (xf,yf)
	| yi<yf = Norte:caminho (xi,yi+1) (xf,yf)
--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Norte:t) = vertical t 
vertical (Sul:t) = vertical t
vertical (_:t) = False
--47
data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [] = (Pos 0 0)
maisCentral ((Pos x y):[]) = (Pos x y)
maisCentral ((Pos x1 y1):(Pos x2 y2):t)
	| (x1^2+y1^2) < (x2^2+y2^2) = maisCentral ((Pos x1 y1):t)
	| otherwise = maisCentral ((Pos x2 y2):t)
--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos a [] = []
vizinhos (Pos x y)  ((Pos x1 y1):t)
	| abs(abs x1 - abs x) == 1 = (Pos x1 y1):vizinhos (Pos x y) t
	| abs(abs y1 - abs y) == 1 = (Pos x1 y1):vizinhos (Pos x y) t
	| otherwise = vizinhos (Pos x y) t
--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada (h:[]) = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):t)
	| x1==x2 = mesmaOrdenada ((Pos x2 y2):t)
	| otherwise = False
--50
data Semaforo = Verde | Amarelo | Vermelho deriving Show
interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (h1:[]) = True
interseccaoOK (Vermelho:h2:t) = interseccaoOK (h2:t)
interseccaoOK (h1:Vermelho:t) = interseccaoOK (h1:t)
interseccaoOK _ = False