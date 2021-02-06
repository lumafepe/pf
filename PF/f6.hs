data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

minimo :: Ord a => BTree a -> a
minimo (Node x Empty d) = x
minimo (Node _ e _) = minimo e

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d

minSmin :: Ord a => BTree a ->(a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = let (m,e') = minSmin e
					   in (m,Node x e' d)

remove :: Ord a => a -> BTree a ->BTree a
remove x Empty = Empty
remove x (Node y e d)
	| x < y = Node y (remove x e) d
	| x > y = Node y e (remove x d)
	| x == y =  Empty
arv2 = Node 7 (Node 5 Empty (Node 6 Empty Empty)) (Node 10 (Node 8 Empty (Node 9 Empty Empty)) (Node 20 Empty Empty))

