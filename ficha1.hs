module Ficha1 where
import Data.Char
perimetro::Double->Double
perimetro r = 3.1415*2*r

dist2::(Double,Double)->(Double,Double)->Double
dist2 (a,b) (c,d) = sqrt ( (a-c)*(a-c)+(b-d)*(b-d) )

primUlt::[a]->(a,a)
primUlt l = (head l,last l)

multiplo::Int->Int->Bool
multiplo m n = mod n m ==0

truncaImpar::[a]->[a]
truncaImpar l
    | mod (length l) 2 == 1 = tail l
    | otherwise = l 
max2::Ord a => a -> a -> a
max2 a b
    | a>b = a
    | otherwise = b
max3::Ord a => a -> a -> a -> a
max3 a b c = foldr (\x ac -> max2 ac x) a [b,c]

--2

nRaizes::Double -> Double -> Double -> Int
nRaizes a b c
    | n<0 = 0
    | n==0 = 1
    | otherwise = 2
    where n = b*b-4*a*c

raizes::Double -> Double -> Double -> [Double]
raizes a b c
    | nraizes == 0 = []
    | nraizes == 1 = [(-1*b)/(2*a)]
    | otherwise = [((-1*b)+sqrt (b*b-4*a*c) )/(2*a),((-1*b)-sqrt (b*b-4*a*c) )/(2*a)]
    where nraizes = nRaizes a b c

--3
{--
type Hora = (Int,Int)

valida::Hora->Bool
valida (h,m) = h<24 && h>=0 && m>=0 && m<60

compara::Hora->Hora->Bool
compara (h1,m1) (h2,m2) = h1>h2 || (h1==h2 && m1>m2)

convMin::Hora->Int
convMin (h,m) = m+60*h

convHora::Int->Hora
convHora m = (div m 60,mod m 60)

difhora::Hora->Hora->Int
difhora a b = if ( c-d )<0 then (-(c-d)) else (c-d)
    where c = convMin a
          d = convMin b

addmin::Int->Hora->Hora
addmin a l = convHora ( convMin l + a )
--}
--4
data Hora = H Int Int deriving (Show,Eq)

valida::Hora->Bool
valida (H h m) = h<24 && h>=0 && m>=0 && m<60

compara::Hora->Hora->Bool
compara (H h1 m1) (H h2 m2) = h1>h2 || (h1==h2 && m1>m2)

convMin::Hora->Int
convMin (H h m) = m+60*h

convHora::Int->Hora
convHora m = (H (div m 60) (mod m 60))

difhora::Hora->Hora->Int
difhora a b = if ( c-d )<0 then (-(c-d)) else (c-d)
    where c = convMin a
          d = convMin b

addmin::Int->Hora->Hora
addmin a l = convHora ( convMin l + a )

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)


next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop a = a==Vermelho

safe :: Semaforo -> Semaforo -> Bool
safe a b = (a==Vermelho || b==Vermelho)

--6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d r) = d*cos (r) 

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d r) = d*sin (r) 

raio :: Ponto -> Double
raio (Polar d r) = d
raio (Cartesiano x y) = sqrt ( x*x+y*y)

angulo :: Ponto -> Double
angulo (Polar d r) = r
angulo p@(Cartesiano x y) = x/(raio p)

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ( (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)   )
    where x1=posx p1
          x2=posx p2
          y1=posy p1
          y2=posy p2
--7

data Figura = Circulo Ponto Double| Rectangulo Ponto Ponto| Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _ = True

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo p1 p2)=[(Cartesiano x1 y1),(Cartesiano x1 y2),(Cartesiano x2 y1),(Cartesiano x2 y2)]
    where x1=posx p1
          x2=posx p2
          y1=posy p1
          y2=posy p2
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

area :: Figura -> Double
area (Triangulo p1 p2 p3) = sqrt (s*(s-a)*(s-b)*(s-c))
    where a = dist p1 p2
          b = dist p2 p3
          c = dist p3 p1
          s = (a+b+c) / 2
area (Circulo c r) = r*r*pi
area (Rectangulo p1 p2) = if s<0 then (-s) else s
    where x1=posx p1
          x2=posx p2
          y1=posy p1
          y2=posy p2
          s=(x2-x1)*(y2-y1)
--8
isLower2 :: Char -> Bool
isLower2 a = a>='a'&& a<='z'
isDigit2 :: Char -> Bool
isDigit2 a = a>='0'&& a<='9'
isAlpha :: Char -> Bool
isAlpha a = not $ isDigit2 a
