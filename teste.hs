intersect::Eq a => [a]->[a]->[a]
intersect l1 l2 = filter (\x -> elem x l2) l1



l1 = [1,2,3,4,5,6,7,8,9]
l2 = [0..3]

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)
a::ConjInt
a=[(1,4),(7,8),(19,19),(21,23)]
elems ::  ConjInt -> [Int]
elems [] = []
elems a = concat $ map (\x -> [(fst x)..(snd x)] ) a 

tails::[a]->[[a]]
tails [] = [[]]
tails m = m:tails (tail m)


geraconj ::  [Int] -> ConjInt
geraconj h = geraconj2 h []

geraconj2:: [Int]->ConjInt->ConjInt
geraconj2 [] a = a
geraconj2 (h:t) [] = geraconj2 t [(h,h)]
geraconj2 (h:t) l
    | h-1 == f = geraconj2 t ((init l)++[(i,h)])
    | otherwise = geraconj2 t (l++[(h,h)])
    where (i,f) = last l


data Contacto = Casa Integer| Trab Integer| Tlm Integer| Email String deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail ::  Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda  
    | elem nome (map (fst) agenda) = map (\x -> if fst(x)==nome then (fst(x),(snd x )++[(Email email)]) else x) agenda
    | otherwise = agenda++[(nome,[(Email email)]) ]

agen = [("Manela",[Email "manela@gmail.com",Tlm 934662832])]


verEmails ::  Nome -> Agenda -> Maybe [String]
verEmails nome agenda 
    | elem nome (map (fst) agenda) = Just (separaEmail gajo)
    | otherwise = Nothing
    where gajo = snd((filter (\x -> fst(x)==nome) agenda)!!0)

separaEmail::[Contacto]->[String]
separaEmail [] = []
separaEmail ((Email x):t) = x:separaEmail t
separaEmail (h:t) = separaEmail t


consulta ::  [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta ((Email x):t) = (a,x:b)
    where (a,b) = consulta t
consulta ((Tlm x):t) = (x:a,b)
    where (a,b) = consulta t
consulta ((Casa x):t) = (x:a,b)
    where (a,b) = consulta t
consulta ((Trab x):t) = (x:a,b)
    where (a,b) = consulta t


consultaIO ::  Agenda -> IO ()
consultaIO x = do nome <- getLine 
                  putStr $ show (snd((filter (\y -> fst(y)==nome) x)!!0))



--4
data RTree a = R a [RTree a] deriving (Show, Eq)

paths ::  RTree a -> [[a]]
paths (R a []) = [[a]]
paths (R a l) =  map (a:) $concat $ map (paths) l

unpaths ::  Eq a => [[a]] -> RTree a
unpaths a = (foldr (\x ac -> (inserepaths x ac)) [] a)!!0

inserepaths::Eq a => [a]->[RTree a]->[RTree a]
inserepaths [] xs = xs
inserepaths (h:t) [] = [R h (inserepaths t [])]
inserepaths (h:t) ((R n l):xs)
    | h==n = xs++[(R n (inserepaths t l))]
    | otherwise = (inserepaths (h:t) xs)++[(R n l)]



ar = R 1 [R 2 [],
          R 3 [R 4 [R 5 [],
                    R 6 []]],
          R 7 []]
exe1 = R 5 [ R 4 [ R 3 [ R 17 []], R 2 [], R 7 []],
                       R 10 [],
                       R 1 [ R 8 [ R 0 [], R 20 [], R 15 [], R 39 [] ],
                             R 12 []]
                     ]


