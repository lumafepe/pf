module Ficha where

import Data.Char

--Exercicio1

perimetro :: Double->Double
perimetro r = 2*r*pi

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ( ( x1-x2 )^2 + ( y1-y2 )^2)

primUlt :: [a]->(a,a)
primUlt list = (head(list),last(list))

multiplo :: Int -> Int -> Bool
multiplo m n = (mod m n == 0)

truncaImpar :: [a] -> [a]
truncaImpar x =
    if mod (length x) 2 == 1
        then tail x
        else [head x]

max2 :: Double -> Double -> Double
max2 x y = 
    if x >= y
        then x
        else y


min2 :: Double -> Double -> Double
min2 x y = 
    if x <= y
        then x
        else y

max3 :: Double -> Double -> Double -> Double
max3 x y z = max2 (max2 x y) z 

--Exercicio 2

nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c = if (b^2 - 4 * a * c)<0 
                then 0
                  else 
                    if (b^2 - 4 * a * c)==0
                    then 1
                    else 2 


raizes :: Double -> Double -> Double -> [Double]
raizes a b c = if (nRaizes a b c)==0
                then [0]
                  else 
                    if (nRaizes a b c)==1
                    then [(-b)/(2 * a)]
                    else  [ ((-b) + sqrt(b^2 - 4 * a * c))/(2 * a) , ((-b) - sqrt(b^2 - 4 * a * c))/(2 * a) ]

--Exercicio 3

type Hora = (Int,Int)

horavalida :: Hora -> Bool
horavalida (h,m) = (h<24 && h>=0 && m>=0 && m<60)

comphora :: Hora -> Hora -> Bool
comphora h1 h2 = convmin h1 > convmin h2  

convmin :: Hora -> Int
convmin (h,m) = h*60+m

convhora :: Int -> Hora
convhora m = (div m 60,mod m 60)

divhoras :: Hora -> Hora -> Int
divhoras h1 h2 = abs( convmin h1 - convmin h2 )

adcminahora :: Hora -> Int -> Hora
adcminahora h m = convhora ( convmin h + m )

--Exercicio 4

data Hora2 = H Int Int deriving (Show,Eq)
{--
horavalida2 :: Hora2 -> Bool
horavalida2 (H h m) = (h<24 && h>=0 && m>=0 && m<60)

comphora2 :: Hora2 -> Hora2 -> Bool
comphora2 h1 h2 = convmin2 h1 > convmin2 h2  

convmin2 :: Hora2 -> Int
convmin2 (H h m) = h*60+m

convhora2 :: Int -> Hora2
convhora2 minutos = 
	let h = div minutos 60;
	    m = mod minutos 60;
    in H h m

divhoras2 :: Hora2 -> Hora2 -> Int
divhoras2 h1 h2 = abs( convmin2 h1 - convmin2 h2 )

adcminahora2 :: Hora2 -> Int -> Hora2
adcminahora2 h m = convhora2 (( convmin2 h )+ m )
--}
--Exercicio 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Vermelho = Verde
next Verde = Amarelo
next Amarelo = Vermelho

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

safe :: Semaforo -> Semaforo -> Bool
safe Verde Verde = False
safe _ _ = True

--Exercicio 6

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

dist2 :: Ponto -> Ponto -> Double
dist2 p1 p2 = dist (posx(p1),posy(p1)) (posx(p2),posy(p2)) 

--Exercicio 7

data Figura = Circulo Ponto Double| Rectangulo Ponto Ponto| Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo p1 r) = False
poligono _ = True

vertices :: Figura -> [Ponto]
vertices (Circulo p d) = []
vertices (Rectangulo p1 p4) = [ p1, (Cartesiano (posx(p1)) (posy(p4))), (Cartesiano (posx(p4)) (posy(p1))) , p4 ]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist2 p1 p2
        b = dist2 p2 p3
        c = dist2 p3 p1
        s = (a+b+c) / 2 
    in sqrt (s*(s-a)*(s-b)*(s-c))
area (Rectangulo p1 p2) = abs( (posx(p1)-posx(p2)) * (posy(p2)-posy(p1)))
area (Circulo p r) = pi * r^2

perimetro2 :: Figura -> Double
perimetro2 (Triangulo p1 p2 p3) = dist2 p1 p2 + dist2 p2 p3 + dist2 p3 p1
perimetro2 (Rectangulo p1 p2) = abs( (posx(p1)-posx(p2)) ) * 2 + abs( (posy(p2)-posy(p1))) * 2
perimetro2 (Circulo p1 r) = 2 * pi * r


--Exercicio 8 

isLower :: Char -> Bool
isLower l = ord(l)>=97 && ord(l)<123

isDigit :: Char -> Bool
isDigit d = ord(d)>=48 && ord(d)<58


