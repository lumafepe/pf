module Ficha3 where


data Hora = H Int Int deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

hval :: Hora -> Bool
hval (H h m) = (h<24 && h>=0) && (m<60 && m>=0)

comphora :: Hora -> Hora -> Bool
comphora (H h1 m1) (H h2 m2) = (h1<h2 || (h1==h2 && m1<m2))

etapaval :: Etapa -> Bool
etapaval (hp,hc) = hval hp && hval hc && comphora hp hc 

viagemval :: Viagem -> Bool
viagemval [] = False
viagemval (a:b:t) = etapaval a && comphora (snd(b)) (fst(a)) && viagemval (b:t) 
viagemval [e] = etapaval e

convmin :: Hora -> Int
convmin (H h m) = h*60+m

convhora :: Int -> Hora
convhora m = (H (div m 60) (mod m 60))


saidachegada :: Viagem -> (Hora,Hora)
saidachegada x = (fst(head x) , snd(last x))

difhoras ::Hora -> Hora ->Hora
difhoras h1 h2 = convhora (convmin h2 - convmin h1)

somahoras :: Hora -> Hora -> Hora
somahoras h1 h2 = convhora(convmin h1 + convmin	h2)

tempodeviagem :: Viagem -> Hora
tempodeviagem [] = (H 0 0)
tempodeviagem ((hp,hc):t) = somahoras (difhoras hp hc) (tempodeviagem t)

tempodeespera :: Viagem -> Hora
tempodeespera [] = (H 0 0)
tempodeespera (h1:[]) = (H 0 0)
tempodeespera ((hp1,hc1):(hp2,hc2):t) = somahoras (difhoras hc1 hp2) (tempodeespera t)

tempototal :: Viagem -> Hora
tempototal a = somahoras (tempodeespera a) (tempodeviagem a)
--2
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d ang) = d * sin(ang)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d ang) = d * cos(ang)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2+y^2)
raio (Polar d ang) = d

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan( y/x )
angulo (Polar d ang) = ang
{--
dist2 :: Ponto -> Ponto -> Double
 dist2 p1 p2 = dist2 (posx(p1),posy(p1)) (posx(p2),posy(p2)) 
--}
type Poligonal = [Ponto]





--3
data Contacto = Casa Integer
		| Trab Integer
		| Tlm Integer
		| Email String 
		deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n s [] = [(n,[Email s ])]
acrescEmail n s ((nome,conts):t) 
 | n == nome = ((n,(Email s):conts):t)
 | otherwise = acrescEmail n s t 

verEmail :: Nome -> Agenda -> Maybe [String]
verEmail n [] =Nothing
verEmail n ((x,l):t) 
	| n ==  x = Just (emails l)
	| otherwise = verEmail n t

emails :: [Contacto] -> [String]
emails [] = []
emails ((Email x):t) = x: emails t
emails (_:t) = emails t

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa n):t) = n : consTelefs t
consTelefs ((Trab n):t) = n : consTelefs t
consTelefs ((Tlm n):t) = n : consTelefs t
consTelefs (_:t) = consTelefs t


casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing
casa n ((nome,conts):t)
	| n==nome = casaconts conts
	| otherwise = casa n t


casaconts :: [Contacto] -> Maybe Integer
casaconts [] = Nothing
casaconts ((Casa n):_) = Just n
casaconts (_:t) = casaconts t             





data Movimento = Credito Float | Debito Float deriving Show
data Data = D Int Int Int deriving Show
data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

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
saldoaux _ [] = 0
saldoaux s ((_,_,(Debito n)):t) = s+n+saldoaux s t
saldoaux s ((_,_,(Credito n)):t) = s+n+saldoaux s t