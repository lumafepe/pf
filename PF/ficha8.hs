
data Frac = F Integer Integer 

normaliza :: Frac->Frac
normaliza (F _ 0) = error "Fuck U"
normaliza (F c b)
	| (a*b)<0 = F (-(abs(c) `div` a)) (abs(b) `div`a)
	| otherwise =F (abs(c) `div` a) (abs(b) `div` a)
	where a = mdc (abs c) (abs b )
mdc :: Integer ->Integer->Integer
mdc  a b 
	| a == b = a
	| a >= b = mdc (a-b) b
	| a < b = mdc (a) (b-a)

instance Eq Frac where
	(F a b) == (F c d) = a*d == c*b
instance Ord Frac where
	x <= y = a*d <= c*b
		where (F a b) = x;
			  (F c d) = y
instance Show Frac where
	--show :: a -> String
	show (F a b) = "("++show a++"/"++show b ++ ")"
instance Num Frac where
	(F a b) + (F c d) =normaliza $ F (a*d+b*c) (b*d)
	(F a b) * (F c d) =normaliza $ F (a*c) (b*d)
	negate (F a b) =normaliza $ F (-a) b
	abs (F a b) =normaliza $ F (abs a) (abs b)
	signum (F a b) =normaliza $ F ((signum a)*(signum b)) 1
	fromInteger n = F n 1

fun :: Frac -> [Frac] -> [Frac]
fun f l = filter (>(2*f)) l 


--3
data Movimento = Credito Float | Debito Float deriving Show
data Data = D Int Int Int 
data Extracto = Ext Float [(Data, String, Movimento)] 

ext1::Extracto
ext1 = (Ext 300 
	[((D 2010 4 5),"DEPOSITO",Credito 2000)
	,((D 2010 8 10),"COMPRA",Debito 37.5)
	,((D 2010 9 1),"LEV",Debito 60)
	,((D 2011 1 7),"JUROS",Credito 100)
	,((D 2011 1 22),"ANUIDADE",Debito 8)])




extValor ::Extracto -> Float -> [Movimento]
extValor (Ext s []) p = []
extValor (Ext s ((_,_,(Debito n )):t)) p 
	| n>p = ((Debito n)):(extValor (Ext s t) p)
	| otherwise = (extValor (Ext s t) p)
extValor (Ext s ((_,_,(Credito n )):t)) p 
	| n>p = ((Credito n)):(extValor (Ext s t) p)
	| otherwise = (extValor (Ext s t) p)


creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ t) = creDeb1 t

creDeb1 :: [(Data, String, Movimento)] -> (Float,Float)
creDeb1 [] = (0,0)
creDeb1 ((_,_,(Credito a)):t) = (a+x,y) where (x,y) = creDeb1 t
creDeb1 ((_,_,(Debito a)):t) = (x,a+y) where (x,y) = creDeb1 t


saldo :: Extracto -> Float
saldo (Ext saldoi l) = saldoaux saldoi l

saldoaux :: Float -> [(Data, String, Movimento)] -> Float
saldoaux s [] = s
saldoaux s ((_,_,(Debito n)):t) = ((saldoaux s t) - (n))
saldoaux s ((_,_,(Credito n)):t) =((saldoaux s t) + (n))




instance Eq Data where
	(D a1 m1 d1)==(D a2 m2 d2) = a1==a2 && m2==m1 && d1==d2
instance Ord Data where
	(D a1 m1 d1)<=(D a2 m2 d2) = a1<a2 || (a1==a2 && m1<m2) || (a1==a2 && m1==m2 && d1<=d2)
instance Show Data where
	show (D a m d) = show(a)++"/"++show(m)++"/"++show(d) 

ordena ::  Extracto -> Extracto
ordena (Ext s l) = Ext s (ordenal l)  

ordenal :: [(Data, String, Movimento)]->[(Data, String, Movimento)]
ordenal [] = []
ordenal (h:t) = insere h (ordenal t)

insere :: (Data, String, Movimento)->[(Data, String, Movimento)]->[(Data, String, Movimento)]
insere h [] = (h:[])
insere (d,desc,mov) ((d1,desc1,mov1):t)
	| d<d1 = (d,desc,mov):(d1,desc1,mov1):t
	| otherwise = (d1,desc1,mov1):insere (d,desc,mov) t

rp :: Int -> Char -> String
rp 0 x = ""
rp n x = x:rp (n-1) x 

instance Show Extracto where
	show (Ext si1 l1)="Saldo anterior: "++show(si)++"\n"++(rp 39 '-')++"\n"++"Data       Descricao   Credito   Debito"++"\n"++(rp 39 '-')++"\n"++(convl l)++(rp 39 '-')++"\n"++"saldo actual: "++show(saldo (Ext si l))
		where (Ext si l) = ordena (Ext si1 l1)


convl :: [(Data,String,Movimento)]->String
convl [] = ""
convl ((d,desc,Credito v):t) = show(d)++(rp (11-(length $ show d )) ' ' )++desc++(rp (12-(length desc )) ' ' )++show(v)++"\n"++convl t
convl ((d,desc,Debito v):t) = show(d)++(rp (11-(length $ show d )) ' ' )++desc++(rp (22-(length desc )) ' ' )++show(v)++"\n"++convl t