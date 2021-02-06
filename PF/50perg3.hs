pp ::[a] -> [a] -> [a]
pp [] [] = []
pp [] (x:xs) = x:pp [] xs
pp (x:xs) (y:ys) = x:pp xs (y:ys) 

ee :: [a] -> Int -> a
ee (h:t) 0 = h
ee (h:t) n = ee t (n-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x) = (last(x)):(reverse'(init(x)))

inits ::[a] -> [[a]]
inits x= reverse' (inits2 (x))
	where inits2 [] = [[]];
		  inits2 x = x:inits2 (init x)

nub :: Eq a => [a] -> [a]
nub x = reverse'(nub2 x [])
	where nub2 [] x = x;
		  nub2 (h:t) x = if elem h x then nub2 t x else nub2 t (h:x)
union :: Eq a => [a] -> [a]->[a]
union [] []=[]
union x [] = x
union [] x = x
union x (y:ys) = if elem y x then union x ys else (union x ys )++[y]                                       

catMaybes ::[Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes ((Just a):t) = a:catMaybes t