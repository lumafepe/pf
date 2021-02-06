import Data.Char
enumFromTo' ::  Int -> Int ->[Int]
enumFromTo' x y 
	| x>y = x:enumFromTo' (x-1) y
	| x<y = x:enumFromTo' (x+1) y
	| otherwise = x:[]
enumFromThenTo' ::  Int -> Int-> Int -> [Int]
enumFromThenTo' x y z
	| x>z = enumFromThenTop x (y-x) z
	| otherwise = enumFromThenTon x (y-x) z
	where
		enumFromThenTop x y z
			| x==z = x:[]
			| x<z = []
			| otherwise = x:enumFromThenTop (x+y) y z 
		enumFromThenTon x y z
			| x==z = x:[]
			| x>z = []
			| otherwise = x:enumFromThenTon (x+y) y z 
pp ::  [a] -> [a] -> [a]
pp x [] = x
pp [] (h:t) = h:pp [] t
pp (h:t) x = h:pp t x

ee ::  [a] -> Int -> a
ee (h:t) n
	| n==0 = h
	| otherwise = ee t (n-1)
reverse' ::  [a] -> [a]
reverse' [] = []
reverse' (h:t) = x++[h] where x= reverse' t 
take' ::  Int -> [a] -> [a]
take' 0 x = []
take' n (h:t) = h:take' (n-1) t
drop' ::  Int -> [a] -> [a]
drop' 0 x = x
drop' n (h:t) = drop' (n-1) t
zip' ::  [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
elem' ::  Eq a => a -> [a] -> Bool
elem' h [] = False
elem' h (x:xs)
	| h==x = True
	| otherwise = elem' h xs 
replicate' ::  Int -> a ->[a]
replicate' n l 
	| n==0 = []
	| otherwise = l:replicate' (n-1) l
intersperse' ::  a -> [a] ->[a]
intersperse' n [] = []
intersperse' n (h:[])=h:[] 
intersperse' n (h:t) = h:n:intersperse' n t

group' ::  Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = replicate x h:group' xs 
	where (x,xs)= group2 (h:t) (1,[]);
group2 ::Eq a => [a] -> (Int,[a])-> (Int,[a]);
group2 [] (n,l) = (n,l);
group2 (h1:[]) (n,l) = (n,l);
group2 (h1:h2:t) (n,l)
	| h1==h2 = group2 (h2:t) (n+1,(h2:t))
	| otherwise = (n,(h2:t))
concat' ::  [[a]] -> [a]
concat' [] = []
concat' (h:t) = h++concat' t 
inits ::  [a] -> [[a]]
inits x = reverse (inits' x)
inits' [] = [[]]
inits' x = x:inits' (init x)
tails ::  [a] -> [[a]]
tails [] = [[]]
tails x = x:tails (tail x)
isPrefixOf ::  Eq a => [a]-> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) 
	| x==y = isPrefixOf xs ys
	| otherwise = False
isSuffixOf ::  Eq a => [a]-> [a] -> Bool
isSuffixOf [] [] = True
isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf x y 
	| last(x) == last(y) = isSuffixOf (init(x)) (init(y))
	| otherwise = False 
isSubsequenceOf ::  Eq a =>[a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)
	| x==y = isSubsequenceOf xs ys
	| otherwise = isSubsequenceOf (x:xs) ys
elemIndices ::  Eq a => a ->[a] -> [Int]
elemIndices a [] = []
elemIndices a x = elemIndices2 a (zip x [0..])
elemIndices2 :: Eq a => a -> [(a,Int)]->[Int]
elemIndices2 a [] = []
elemIndices2 a ((v,n):t)
	| a == v = n:elemIndices2 a t
	| otherwise =elemIndices2 a t
delete' ::  Eq a => a -> [a]-> [a]
delete' x [] = []
delete' x (h:t)
	| x==h = t
	| otherwise = h:delete' x t
dd ::  Eq a => [a] -> [a]-> [a]
dd [] _ = []
dd x [] = x
dd x (y:ys) = dd (delete' y x) ys
union ::  Eq a => [a] -> [a]-> [a]
union [] x = x
union x [] = x
union x y = x++union2 x y
union2 x [] = []
union2 x (y:ys)
	| (elem y x) = union2 x ys
	| otherwise = y:union2 x ys
intersect ::  Eq a => [a] ->[a] -> [a]
intersect [] _ = []
intersect (x:xs) y 
	| (elem x y) = x:intersect xs y
	| otherwise = intersect xs y
insert ::  Ord a => a -> [a]-> [a]
insert n [] = n:[]
insert n (h:t)
	| n<h = n:h:t
	| otherwise =h:insert n t 
unwords' ::  [String] -> String
unwords' []=[]
unwords' (h:[]) = h
unwords' (h:t) = h++" "++unwords' t 
unlines' ::  [String] -> String
unlines' [] = []
unlines' (h:t) = h++"\n"++unlines' t 
pMaior ::  Ord a => [a] -> Int
pMaior (h:[]) = 0
pMaior (h:t) = pMaior2 h t 1 1
pMaior2 :: Ord a => a->[a]->Int ->Int-> Int
pMaior2 v [] pv pa = pv
pMaior2 v (h:t) pv pa
	| v>h = pMaior2 v t pv (pa+1)
	| otherwise = pMaior2 h  t pa (pa+1)
temRepetidos ::  Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:[])= False
temRepetidos (h:t)
	| (elem h t) = True
	| otherwise =temRepetidos t
algarismos ::  [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
	| ord(h)>ord('0') && ord(h)<ord('9') = h:algarismos t
	| otherwise = algarismos t
posImpares ::  [a] -> [a]
posImpares [] = []
posImpares (h1:[])=[]
posImpares (h1:h2:t)=h2:posImpares t
posPares ::  [a] -> [a]
posPares (h1:[])=h1:[]
posPares (h1:h2:t)=h1:posPares t
isSorted ::  Ord a => [a] -> Bool
isSorted [] = True
isSorted (h:[]) = True
isSorted (h1:h2:t) = h1<=h2 && isSorted (h2:t)
iSort ::  Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)
l=[('b',2), ('a',4), ('c',1)]
menor ::  String -> String -> Bool
menor [] [] = False
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) = ord(x)<=ord(y) && menor xs ys 
elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((h,hs):t) = a==h || elemMSet a t
lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((h,hs):t) = hs+lengthMSet t
converteMSet ::  [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((h,hs):t) = replicate hs h ++ converteMSet t
insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = (a,1):[]
insereMSet a ((h,hs):t) 
	| a==h = (h,(hs+1)):t
	| otherwise = (h,hs):insereMSet a t
removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((h,hs):t)
	| a== h = t
	| otherwise = (h,hs):removeMSet a t
constroiMSet ::  Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = a:constroiMSet x2 where (a,x2)=constroiMSet2 h t (h,1) 
constroiMSet2 ::  Ord a => a-> [a] -> (a,Int) -> ((a,Int),[a])
constroiMSet2 a [] (h1,n) = ((h1,n),[])
constroiMSet2 a (h:t) (h1,n)
	| a==h = constroiMSet2 a t (h1,n+1)
	| otherwise = ((h1,n),(h:t))
partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers x = partitionEithers2 x ([],[])
partitionEithers2 [] (a,b)= (reverse(a),reverse(b))
partitionEithers2 ((Left h):t) (a,b) = partitionEithers2 t (h:a,b)
partitionEithers2 ((Right h):t) (a,b) = partitionEithers2 t (a,h:b)
catMaybes ::  [Maybe a] -> [a]
catMaybes []= []
catMaybes ((Nothing):t) = catMaybes t
catMaybes ((Just h):t) = h:catMaybes t
data Movimento = Norte | Sul | Este | Oeste deriving Show
posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t
posicao (x,y) (Este:t) = posicao (x+1,y) t
caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (xf,yf) 
	| x<xf = Este:caminho (x+1,y) (xf,yf)
	| x>xf = Oeste:caminho (x-1,y) (xf,yf)
	| y<yf = Norte:caminho (x,y+1) (xf,yf)
	| y>yf = Sul:caminho (x,y-1) (xf,yf)
	| otherwise = []
vertical ::  [Movimento] -> Bool
vertical [] = True
vertical (Norte:t)=vertical t
vertical (Sul:t)=vertical t
vertical (_:t)=False
data Posicao = Pos Int Int deriving Show
maisCentral ::  [Posicao] -> Posicao
maisCentral (h:[]) = h
maisCentral ((Pos x1 y1):(Pos x2 y2):t)
	| x1^2+y1^2<=x2^2+y2^2 = maisCentral ((Pos x1 y1):t)
	| otherwise = maisCentral ((Pos x2 y2):t)
vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) [] = []
vizinhos (Pos x y) ((Pos x1 y1):t)
	| (x==x1 && (y==y1+1 || y==y1-1)) || (y==y1 && (x==x1+1 || x==x1-1)) = (Pos x1 y1):vizinhos (Pos x y) t
	| otherwise = vizinhos (Pos x y) t
mesmaOrdenada ::  [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada ((Pos x y):[]) = True
mesmaOrdenada ((Pos x y):(Pos x1 y1):t) = y==y1 && mesmaOrdenada ((Pos x1 y1):t)
data Semaforo = Verde | Amarelo | Vermelho deriving Show
interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (x:[]) = True
interseccaoOK (Vermelho:t)=interseccaoOK t
interseccaoOK (x:Vermelho:t)=interseccaoOK (x:t)
interseccaoOK (x:y:t)=False