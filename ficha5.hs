module Ficha5 where

any :: (a -> Bool) -> [a] -> Bool
any f l = foldr (\x ac -> x || ac) False (map f l)

zipWith2 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith2 f [] _ = []
zipWith2 f _ [] = []
zipWith2 f (h:t) (h1:t1) = (f h h1):zipWith2 f t t1 

takeWhile2 :: (a->Bool) -> [a] -> [a]
takeWhile2 f [] = []
takeWhile2 f (h:t)
	| f h = h:takeWhile2 f t
	| otherwise = []


dropWhile2 :: (a->Bool) -> [a] -> [a]
dropWhile2 f [] = []
dropWhile2 f (h:t)
	| f h = (h:t)
	| otherwise = dropWhile2 f t

span :: (a-> Bool) -> [a] -> ([a],[a])
span f (h:t)