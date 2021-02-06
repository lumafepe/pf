enumFromTo' ::  Int -> Int ->[Int]
enumFromTo' x y 
	| x>=y = []
	| x<y = x:enumFromTo' (x+1) y

pp ::  [a] -> [a] -> [a]
pp [] x = x
pp x [] = x
pp (h:t) x = h:pp t x
reverse' ::  [a] -> [a]
reverse' []=[]
reverse' (h:t) = x++[h] where x= reverse' t
take' ::  Int -> [a] -> [a]
take' n [] = []
take' 0 l = []
take' n (h:t) = h:take' (n-1) t
group ::  Eq a => [a] -> [[a]]
group [] = []
group (h:t) = replicate n h :group nt where (n,nt) = group2 h 1 t  
group2 :: Eq a => a -> Int -> [a] ->(Int,[a]) 
group2 a n [] = (n,[])
group2 a n (h:t) 
	| a==h = group2 a (n+1) t
	| otherwise = (n,(h:t))
concate ::  [[a]] -> [a]
concate []=[]
concate (h:t) = h++concate t
inits ::  [a] -> [[a]]
inits [] = [[]]
inits x = inits(init x)++[x]
tails ::  [a] -> [[a]]
tails []=[[]]
tails x = x:tails (tail x)
isSubsequenceOf ::  Eq a =>[a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) 
	| x==y = isSubsequenceOf xs ys
	| otherwise = isSubsequenceOf (x:xs) ys
elemIndices ::  Eq a => a ->[a] -> [Int]
elemIndices a x = elemIndices2 a 0 x 
elemIndices2 :: Eq a => a -> Int -> [a] -> [Int]
elemIndices2 x n [] = []
elemIndices2 x n (h:t)
	| h==x = n:elemIndices2 x (n+1) t
	| otherwise = elemIndices2 x (n+1) t

nub ::  Eq a => [a] -> [a]
nub [] = []
nub (h:t) = h:nub (remove h t)

remove :: Eq a => a-> [a] -> [a]
remove a [] = []
remove a (h:t) 
	| a==h = remove a t
	| otherwise = h:remove a t