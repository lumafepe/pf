type Mat a = [[a]]
dimOk :: Mat a -> Bool
dimOk [] = False
dimOk (h:[]) = True
dimOk (x:y:t) = length(x) == length(y) && dimOk (y:t)

b=[[1,2,3],[1,2,3],[1,2,3]]


dimOk2 :: Mat a -> Bool
dimOk2 m = aux (map length m )
    where aux (0:_) = False;
          aux (x:xs) = (filter (/=x) xs) == []

dimMat :: Mat a -> (Int,Int)
dimMat m@(h:t) = (length (m),length(h))

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (x:xs) (y:ys) = (zipWith (+) x y):addMat xs ys

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = map head m: transpose (map tail m) 

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] _ = []
multMat (h:t) x = aux h x : multMat t x
    where aux h ([]:_)= []
          aux h x = sum(zipWith (*) h (map head x)) : aux h (map tail x)