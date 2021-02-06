
data BTree a = Empty| Node a (BTree a) (BTree a) deriving Show

btree = Node 2 (Node 3 (Node 4 Empty Empty) (Node 1 Empty Empty)) (Node 6 Empty (Node 2 Empty Empty))


altura :: BTree a -> Int
altura (Empty) = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

contaNodos :: BTree a -> Int
contaNodos (Empty) = 0
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d

folhas :: BTree a -> Int
folhas (Empty) = 0
folhas (Node x Empty Empty) = 1
folhas (Node x e d) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune 0 (Node x e d) = Empty
prune n (Empty) = Empty
prune n (Node x e d) = (Node x (prune (n-1) e) (prune (n-1) d))  


path :: [Bool] -> BTree a -> [a]
path [] _ = []
path (False:t) (Node x e d) = x:path t e
path (True:t) (Node x e d) = x:path t d


mirror :: BTree a -> BTree a
mirror (Node x e d) = (Node x (mirror d) (mirror e)) 
mirror Empty = Empty

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node x1 e1 d1) = Node (f x x1) (zipWithBT f e e1) (zipWithBT f d d1)
zipWithBT f _ _ = Empty