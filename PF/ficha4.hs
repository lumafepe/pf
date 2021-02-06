import Data.Char

digitAlpha :: String->(String,String)
digitAlpha [] = ([],[])
digitAlpha l = digalp l ([],[])
	where digalp [] (a,b) = (a,b);
		  digalp (h:t) (a,b) 
		  	| isDigit h = digalp t (h:a,b)
		  	| isAlpha h = digalp t (a,h:b)
		  	| otherwise = digalp t (a,b)

digitAlpha' :: String ->(String,String)
digitAlpha' [] = ([],[])
digitAlpha' (h:t) 
	| isDigit h = (h:l1,l2)
	| isAlpha h = (l1,h:l2)
  	| otherwise = (l1,l2)
  where (l1,l2)= digitAlpha' t

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) 
	| h<0 = (n+1,z,p)
	| h==0 = (n,z+1,p)
	| otherwise = (n,z,p+1)
	where (n,z,p)=nzp t

divMod1 :: Integral a=> a -> a -> (a,a)
divMod1 d r = divMod2 d r (0,0)
	where divMod2 ma m (a,b) 
			| d-r>=0 = divMod2 (d-r) r (a+1,b)
			| otherwise = (a,d)
maxSumInit :: (Num a ,Ord a) => [a] -> a
maxSumInit l = fst (msi l (0,0)) 
	where msi [] (max,sun) = (max,sun);
		msi (h:t) (max,sun) 
			| max< h+sun = msi t (h+sun,h+sun)
			| otherwise = msi t (max,h+sun)



fib :: Int -> Int
fib x = fib2 x 0 1
	where fib2 x n1 n2 
			| x == 0 = n1
			| otherwise = fib2 (x-1) n2 (n1+n2)

