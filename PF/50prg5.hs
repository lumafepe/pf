import Data.Char
myenumFromTo ::  Int -> Int ->[Int]
myenumFromTo x y
	| x==y = x:[]
	| x<y = x:myenumFromTo (x+1) y
	| x>y = x:myenumFromTo (x-1) y
--2
myenumFromThenTo ::  Int -> Int-> Int -> [Int]
myenumFromThenTo x y z
	| x<=z = myenumFromThenTop x (y-x) z
	| otherwise = myenumFromThenTon x (y-x) z

myenumFromThenTop :: Int -> Int-> Int -> [Int]
myenumFromThenTop x y z 
	| x<z = x:myenumFromThenTop (x+y) y z
	| x>z = []
	| x==z = x:[]
myenumFromThenTon :: Int -> Int-> Int -> [Int]
myenumFromThenTon x y z 
	| x>z = x:myenumFromThenTon (x+y) y z
	| x<z = []
	| x==z = x:[]
--3
pp ::  [a] -> [a] -> [a]
pp [] [] = []
pp [] (h:t) = h:pp [] t 
pp h [] = h
pp (h:t) x = h:pp t x
--4
ee ::  [a] -> Int -> a
ee (h:t) p
	| p==0 = h
	| otherwise = ee t (p-1)
--5
myreverse ::  [a] -> [a]
myreverse [] = []
myreverse (h:t) = x++[h] where x=myreverse t 
--6
mytake ::  Int -> [a] -> [a]
mytake n l =tak n l []
tak::Int -> [a] ->[a]->[a]
tak 0 _ x = x
tak n (h:t) x = tak (n-1) t (x++[h]) 
--7
mydrop ::  Int -> [a] -> [a]
mydrop _ []=[]
mydrop 0 x = x
mydrop n (h:t) = mydrop (n-1) t 
--8
myzip ::  [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y):myzip xs ys
--9
myelem ::  Eq a => a -> [a] ->Bool
myelem n [] = False
myelem n (h:t)
	| n==h = True
	| otherwise = myelem n t
--10
myreplicate ::  Int -> a ->[a]
myreplicate 0 x = []
myreplicate n x = x:myreplicate (n-1) x
--11
myintersperse ::  a -> [a] ->[a]
myintersperse n [] = []
myintersperse n (h:[]) = h:[]
myintersperse n (h:t) = h:n:myintersperse n t 
--12
mygroup ::  Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h:t) = replicate x h :mygroup r where (x,r) = mygroup2 (h:t) 0
mygroup2 :: Eq a => [a]->Int->(Int,[a])
mygroup2 [] n = (n,[])
mygroup2 (h:[]) n = (1,[])
mygroup2 (h:h2:[]) n 
	| h==h2 = (n+2,[])
	| otherwise = (n+1,(h2:[]))
mygroup2 (h:h2:t) n 
	| h==h2 = mygroup2 (h2:t) (n+1)
	| otherwise = (n+1,h2:t)
--13
myconcat ::  [[a]] -> [a]
myconcat []=[]
myconcat (h:t) = h++myconcat t
--14
inits :: [a] -> [[a]]
inits [] = [[]]
inits x = inits (init(x))++[x]
--15
tails ::  [a] -> [[a]]
tails []=[[]]
tails x = x:tails (tail x)
--16
isPrefixOf ::  Eq a => [a]-> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h1:t1) (h2:t2) 
	| h1==h2 = isPrefixOf t1 t2
	| otherwise = False
--17
isSuffixOf ::  Eq a => [a]-> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf x y 
	| last(x) == last(y) = isSuffixOf (init(x)) (init(y))
	| otherwise = False
--18
isSubsequenceOf ::  Eq a =>[a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) 
	| x==y = isSubsequenceOf xs ys
	| otherwise = isSubsequenceOf (x:xs) ys
--19
elemIndices ::  Eq a => a ->[a] -> [Int]
elemIndices x l = elemIndices2 x (zip l [0..])
	where elemIndices2 x [] = [];
		  elemIndices2 x (h:t)
			| x == fst(h) = snd(h):elemIndices2 x t
			| otherwise = elemIndices2 x t 
--20
nub ::  Eq a => [a] -> [a]
nub a = reverse (nub2 a [])
nub2 :: Eq a => [a] ->[a]->[a]
nub2 [] a = a
nub2 (h:t) a 
	| (elem h a) = nub2 t a
	| otherwise = nub2 t (h:a)
--21
mydelete ::  Eq a => a -> [a]-> [a]
mydelete _ [] = []
mydelete n (h:t)
	| n==h = t
	| otherwise =h:mydelete n t
--22
dd ::  Eq a => [a] -> [a]-> [a]
dd x [] = x
dd [] _ = []
dd x (y:ys) = dd nx ys where nx = dd2 x y

dd2 :: Eq a => [a] -> a-> [a]
dd2 [] y = []
dd2 (h:t) y 
	| h==y = t
	| otherwise = h:dd2 t y
--23
union ::  Eq a => [a] -> [a]-> [a]
union x [] = x
union x y = x++union2 x y
union2 ::  Eq a => [a] -> [a]-> [a]
union2 x [] = []
union2 x (h:t)
	| (elem h x) = union2 x t
	| otherwise = h:(union2 x t)
--24
intersect ::  Eq a => [a] ->[a] -> [a]
intersect [] _ = [] 
intersect (x:xs) y 
	| (elem x y) = x:intersect xs y
	| otherwise = intersect xs y 
--25
insert ::  Ord a => a -> [a]-> [a]
insert n [] = [n]
insert n (h:t) 
	| n<h = n:h:t
	| otherwise = h:insert n t
--26
myunwords ::  [String] -> String
myunwords [] = []
myunwords (h:[]) = h
myunwords (h:t) = h++" "++myunwords t
--27
myunlines ::  [String] -> String
myunlines [] = []
myunlines (h:t) = h++"\n"++myunlines t
--28
pMaior ::  Ord a => [a] -> Int
pMaior (h:t) = max h t 0 0
	where max :: Ord a => a->[a]->Int->Int->Int;
		  max _ [] n a= n+1;
		  max m (h:t) n a 
		  	| m<h = max h t a (a+1)
		  	| otherwise = max m t n (a+1)
--29
temRepetidos ::  Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) 
	| elem h t = True
	| otherwise =temRepetidos t
--30
algarismos ::  [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
	| ord(h) > ord('0')&& ord(h) < ord('9') = h:algarismos t
	| otherwise = algarismos t 
--31
posImpares ::  [a] -> [a]
posImpares x = pI (zip x [0..]) 
	where pI :: [(a,Int)] -> [a];
		  pI [] = [];
		  pI ((n,p):t)
		  	| mod p 2 == 1 = n:pI t
		  	| otherwise= pI t
--32
posPares ::  [a] -> [a]
posPares x = pI (zip x [0..]) 
	where pI :: [(a,Int)] -> [a];
		  pI [] = [];
		  pI ((n,p):t)
		  	| mod p 2 == 0 = n:pI t
		  	| otherwise= pI t
--33
isSorted ::  Ord a => [a] -> Bool
isSorted [] = True
isSorted (h:[])=True
isSorted (h:h1:t)
	| h<=h1 = isSorted (h1:t)
	| otherwise = False
--34
iSort ::  Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)
--35
menor ::  String -> String -> Bool
menor [] [] = False
menor [] _ = True
menor (x:xs) (y:ys) 
	| ord(x) <= ord(y) = menor xs ys
	| otherwise = False
--36
elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((h,n):t)
	| a==h = True
	| otherwise =elemMSet a t
--37
lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((h,n):t) = n+lengthMSet t
--38
converteMSet ::  [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((h,n):t) = (replicate n h )++(converteMSet t)
--39
insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet b [] = ((b,1):[])
insereMSet b ((h,n):t) 
	| b==h = (h,(n+1)):t
	| otherwise = (h,n):insereMSet b t
--40
removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet b [] = []
removeMSet b ((h,n):t)
	| b==h = t
	| otherwise = (h,n):removeMSet b t
--41
constroiMSet ::  Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (h,r):constroiMSet(tn) where (r,tn) = constroiMSet2 h t 1

constroiMSet2 :: Ord a => a -> [a]-> Int -> (Int,[a])
constroiMSet2 n [] r = (r,[]) 
constroiMSet2 n (h:t) r 
	| n==h = constroiMSet2 n t (r+1)
	| otherwise = (r,(h:t))
--42
partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers x = (reverse a,reverse b) where (a,b)=partitionEithers2 x ([],[])
partitionEithers2 :: [Either a b] -> ([a],[b]) -> ([a],[b])
partitionEithers2 [] (a,b)= (a,b)
partitionEithers2 ((Left h):t) (a,b) = partitionEithers2 t ((h:a),b)
partitionEithers2 ((Right h):t) (a,b) = partitionEithers2 t (a,(h:b))
--43
catMaybes ::  [Maybe a] -> [a]
catMaybes []=[]
catMaybes ((Nothing):t) = catMaybes t
catMaybes ((Just a):t) = a:catMaybes t
--44
data Movimento = Norte | Sul | Este | Oeste deriving Show
posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao	(x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t
posicao (x,y) (Este:t) = posicao (x+1,y) t
--45
caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (xf,yf)
	| y<yf = Norte:caminho (x,y+1) (xf,yf)
	| y>yf = Sul:caminho (x,y-1) (xf,yf)
	| x>xf = Oeste:caminho (x-1,y) (xf,yf)
	| x<xf = Este:caminho (x+1,y) (xf,yf)
	| otherwise =[]
--46
vertical ::  [Movimento] -> Bool
vertical [] = False
vertical (Norte:[]) = True
vertical (Sul:[]) = False
vertical (Norte:t) = vertical t
vertical (Sul:t) = vertical t
vertical (_:t) = False 
--47
data Posicao = Pos Int Int deriving Show
maisCentral ::  [Posicao] -> Posicao
maisCentral ((Pos x y):[]) = Pos x y
maisCentral ((Pos x1 y1):(Pos x2 y2):t)
	| abs(x1*x1+y1*y1) < abs(x2*x2+y2*y2) = maisCentral ((Pos x1 y1):t)
	| otherwise = maisCentral ((Pos x2 y2):t)
--48
vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y) ((Pos x1 y1):t)
	| abs(x)-abs(x1)<=1 && y==y1 && x/=x1 = (Pos x1 y1):vizinhos (Pos x y) t
	| abs(y)-abs(y1)<=1 && x==x1 && y/=y1 = (Pos x1 y1):vizinhos (Pos x y) t
	| otherwise = vizinhos (Pos x y) t 
--49
mesmaOrdenada ::  [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada (x:[]) = True
mesmaOrdenada ((Pos x y):(Pos x1 y1):t)
	| y==y1 = mesmaOrdenada ((Pos x y):t)
	| otherwise = False
--50
data Semaforo = Verde | Amarelo | Vermelho deriving Show
interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (h:[]) = True
interseccaoOK (Vermelho:x:t)=interseccaoOK (x:t)
interseccaoOK (x:Vermelho:t) = interseccaoOK (x:t)
interseccaoOK (x:y:t) = False