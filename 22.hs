data LTree a = Tip a | Fork (LTree a) (LTree a)


b::LTree Int
b = Fork (Fork (Tip 7) (Tip 1)) (Tip 2)

instance Show a => (Show (LTree a)) where
	show (Fork e d) = "."++show e ++" \n"++"."++show d
	show (Tip n) = show n 
