module Ficha1 where
import Data.Char

perimetroCirc :: Float -> Float  -- Exercicio 1
perimetroCirc x = 2 * pi * x

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

primUlt :: [a] -> (a, a)
primUlt x = (head x, last x)

multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

truncaImpar :: [a] -> [a]
truncaImpar x
    | even (length x) = x
    | otherwise = tail x

max2 :: Int -> Int -> Int
max2 = max

max3 :: Int -> Int -> Int -> Int
max3 x y = max2 (max2 x y)

nRaizes :: Float -> Float -> Float -> Int  -- Exercicio 2
nRaizes x y z
    | (y^2 - 4*x*z) < 0 = 0
    | (y^2 - 4*x*z) > 0 = 2
    | (y^2 - 4*x*z) == 0 = 1

raizes :: Float -> Float -> Float -> [Float]
raizes x y z
    | nRaizes x y z == 0 = []
    | nRaizes x y z == 1 = [(-y) / (2 * x)]
    | nRaizes x y z == 2 = [((-y) + sqrt (y^2 - 4 * x * z)) / (2 * x), ((-y) - sqrt (y^2 - 4 * x * z)) / (2 * x)]

ahora :: (Int, Int) -> Bool  -- Exercicio 3
ahora (x, y) = (0 <= x && x <= 23) && (0 <= y && y <= 59)

bhora :: (Int, Int) -> (Int, Int) -> Bool
bhora (x1, y1) (x2, y2)
    | x1 == x2 = y1 > y2
    | otherwise = x1 > x2

chora :: (Int, Int) -> Int
chora (x, y) = (x * 60) + y

dhora :: Int -> (Int, Int)
dhora x = (mod (div x 60) 24, mod x 60)

ehora :: (Int, Int) -> (Int, Int) -> Int
ehora (x1, y1) (x2, y2)
    | bhora (x1, y1) (x2, y2) = chora (x1, y1) - chora (x2, y2)
    | otherwise = chora (x2, y2) - chora (x1, y1)

fhora :: Int -> (Int, Int) -> (Int, Int)
fhora m (x, y) = dhora (chora (x + div m 60, y + mod m 60))

data Hora = H Int Int deriving (Show,Eq)  -- Exercicio 4

aHora :: Hora -> Bool  
aHora (H x y) = (0 <= x && x <= 23) && (0 <= y && y <= 59)

bHora :: Hora -> Hora -> Bool
bHora (H x1 y1) (H x2 y2)
    | x1 == x2 = y1 > y2
    | otherwise = x1 > x2

cHora :: Hora -> Int
cHora (H x y) = (x * 60) + y

dHora :: Int -> Hora
dHora x = H (mod (div x 60) 24) (mod x 60)

eHora :: Hora -> Hora -> Int
eHora (H x1 y1) (H x2 y2)
    | bhora (x1, y1) (x2, y2) = cHora (H x1 y1) - cHora (H x2 y2)
    | otherwise = cHora (H x2 y2) - cHora (H x1 y1)

fHora :: Int -> Hora -> Hora
fHora m (H x y) = dHora (cHora (H (x + div m 60) (y + mod m 60)))

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)  -- Exercicio 5

next :: Semaforo -> Semaforo  
next x
    | x == Verde = Amarelo
    | x == Amarelo = Vermelho
    | x == Vermelho = Verde

stop :: Semaforo -> Bool
stop x
    | x == Vermelho = True
    | otherwise = False

safe :: Semaforo -> Semaforo -> Bool
safe x y
    | (x == Vermelho) || (y == Vermelho) = True
    | otherwise = False

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)  -- Exercicio 6

posx :: Ponto -> Double  
posx (Cartesiano x y) = x
posx (Polar x y) = cos y * x

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar x y) = sin y * x

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar x y) = x

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)  -- atan = tan^-1
angulo (Polar x y) = y

distancia :: Ponto -> Ponto -> Double
distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
distancia (Polar x1 y1) (Polar x2 y2) = sqrt ((posx (Polar x1 y1)-posx (Polar x2 y2))^2 + (posy (Polar x1 y1)-posy (Polar x2 y2))^2)

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)  -- Exercicio 7

poligono :: Figura -> Bool
poligono (Circulo (Cartesiano x y) r) = False
poligono (Circulo (Polar x alfa) r) = False
poligono _ = True

vertices :: Figura -> [Ponto]
vertices (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = [Cartesiano x1 y1, Cartesiano x1 y2, Cartesiano x2 y1, Cartesiano x2 y2]
vertices (Rectangulo (Polar x1 alfa) (Polar x2 beta)) = [Polar x1 alfa, Polar x1 (angulo (Cartesiano x1 (posy (Polar x2 beta)))), Polar x2 (angulo (Cartesiano x2 (posy (Polar x1 alfa)))), Polar x2 beta]
vertices (Triangulo x y z) = [x, y, z]
vertices _ = []

area :: Figura -> Double
area (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) =
    let a = dist (x1, y1) (x2, y2)
        b = dist (x2, y2) (x3, y3)
        c = dist (x3, y3) (x1, y1)
        s = (a+b+c) / 2  -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c))  -- formula de Heron
area (Triangulo (Polar x1 alfa) (Polar x2 beta) (Polar x3 gama)) = 
    let a = dist (posx (Polar x1 alfa), posy (Polar x1 alfa)) (posx (Polar x2 beta), posy (Polar x2 beta))
        b = dist (posx (Polar x2 beta), posy (Polar x2 beta)) (posx (Polar x3 gama), posy (Polar x3 gama))
        c = dist (posx (Polar x3 gama), posy (Polar x3 gama)) (posx (Polar x1 alfa), posy (Polar x1 alfa))
        s = (a+b+c) / 2  -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c))  -- formula de Heron
area (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = abs (x1-x2) * (y1-y2)
area (Rectangulo (Polar x1 alfa) (Polar x2 beta)) = 
    let b = abs (posx (Polar x1 alfa) - posx (Polar x2 beta))
        h = abs (posy (Polar x1 alfa) - posy (Polar x2 beta))
    in b*h
area (Circulo (Cartesiano x1 y1) r) = pi*r^2
area (Circulo (Polar x alfa) r) = pi*r^2 

perimetro :: Figura -> Double
perimetro (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) =
    let a = dist (x1, y1) (x2, y2)
        b = dist (x2, y2) (x3, y3)
        c = dist (x3, y3) (x1, y1)
    in a+b+c
perimetro (Triangulo (Polar x1 alfa) (Polar x2 beta) (Polar x3 gama)) = 
    let a = dist (posx (Polar x1 alfa), posy (Polar x1 alfa)) (posx (Polar x2 beta), posy (Polar x2 beta))
        b = dist (posx (Polar x2 beta), posy (Polar x2 beta)) (posx (Polar x3 gama), posy (Polar x3 gama))
        c = dist (posx (Polar x3 gama), posy (Polar x3 gama)) (posx (Polar x1 alfa), posy (Polar x1 alfa))
    in a+b+c
perimetro (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = (2 * abs (x1-x2)) + (2 * abs (y1-y2))
perimetro (Rectangulo (Polar x1 alfa) (Polar x2 beta)) = 
    let b = abs (posx (Polar x1 alfa) - posx (Polar x2 beta))
        h = abs (posy (Polar x1 alfa) - posy (Polar x2 beta))
    in 2*b + 2*h
perimetro (Circulo (Cartesiano x1 y1) r) = 2*pi*r
perimetro (Circulo (Polar x alfa) r) = 2*pi*r

isLower' :: Char -> Bool -- Exercicio 8
isLower' x = (ord x >= ord 'a') && (ord x <= ord 'z')

isDigit' :: Char -> Bool
isDigit' x = (ord x >= ord '0') && (ord x <= ord '9')

isAlpha' :: Char -> Bool
isAlpha' x = ((ord x >= ord 'A') && (ord x <= ord 'Z')) || isLower' x

toUpper' :: Char -> Char
toUpper' x
    | isAlpha' x = x
    | isLower' x = chr (ord x - (ord 'a' - ord 'A'))

intToDigit' :: Int -> Char
intToDigit' x = chr (x + ord '0')

--works for hexadecimal digits: '0'...'9' && ('a'...'f' || 'A'...'F')
digitToInt' :: Char -> Int
digitToInt' x
    | ord x - ord '0' >= 0 && ord x - ord '9' <= 0 = ord x - ord '0'
    | ord x - ord 'a' >= 0 && ord x - ord 'f' <= 0 = ord x - ord 'a' + 10
    | ord x - ord 'A' >= 0 && ord x - ord 'F' <= 0 = ord x - ord 'A' + 10