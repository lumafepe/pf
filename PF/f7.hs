data ExpInt = Const Int
			| Simetrico ExpInt
			| Mais ExpInt ExpInt
			| Menos ExpInt ExpInt
			| Mult ExpInt ExpInt 
	deriving Show
calcula :: ExpInt -> Int
calcula (Mais e d) = calcula(e)+calcula(d)
calcula (Mult e d) = calcula(e)*calcula(d)
calcula (Menos e d) = calcula(e)-calcula(d)
calcula (Simetrico e)= (-calcula(e))
calcula (Const e) = e

infixa :: ExpInt -> String
infixa (Mais e d) = "("++infixa(e)++" + "++infixa(d)++")"
infixa (Mult e d) = "("++infixa(e)++" * "++infixa(d)++")"
infixa (Menos e d) = "("++infixa(e)++" - "++infixa(d)++")"
infixa (Simetrico e) ="("++" -"++infixa(e)++")"
infixa (Const e) = show e

posfixa :: ExpInt -> String
posfixa (Mais e d) = (posfixa e) ++ " -"
posfixa (Mult e d) = (posfixa e) ++ " " ++ (posfixa d)++" *"
posfixa (Menos e d) = (posfixa e) ++ " " ++ (posfixa d)++" -"
posfixa (Simetrico e) = (posfixa e) ++ " ~"
posfixa (Const e) = show e

data RTree a = R a [RTree a] deriving Show

arv = R 5 [R 4 [R 3 [R 17 []],R 2 [],R 7 []],R 10 [],R 1 [R 8 [R 0 [],R 20 [],R 15 [],R 39 []],R 12 []]]
soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)

prune :: Int -> RTree a -> RTree a
prune n (R x l) | n==1 = R x []
				| n>1 = R x (map (prune (n-1)) l )

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

larv = Fork (Fork (Tip 7) (Tip 8)) (Fork (Tip 4) (Fork (Tip 1) (Tip 6)))

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = (listaLT e)++(listaLT d)

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d) 

data 