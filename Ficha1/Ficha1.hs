perimetro :: Float -> Float
perimetro x = 2 * pi * x

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

nRaizes :: Float -> Float -> Float -> Int
nRaizes x y z
    | (y^2 - 4*x*z) < 0 = 0
    | (y^2 - 4*x*z) > 0 = 2
    | (y^2 - 4*x*z) == 0 = 1

raizes :: Float -> Float -> Float -> [Float]
raizes x y z
    | nRaizes x y z == 0 = []
    | nRaizes x y z == 1 = [(-y) / (2 * x)]
    | nRaizes x y z == 2 = [((-y) + sqrt (y^2 - 4 * x * z)) / (2 * x), ((-y) - sqrt (y^2 - 4 * x * z)) / (2 * x)]

ahora :: (Int, Int) -> Bool
ahora (x, y) = (0 <= x && x <= 23) && (0 <= y && y <= 59)

bhora :: (Int, Int) -> (Int, Int) -> Bool
bhora (x1, y1) (x2, y2)
    | x1 == x2 = y1 > y2
    | otherwise = x1 > x2

chora :: (Int, Int) -> Int
chora (x, y) = (x * 60) + y

dhora :: Int -> (Int, Int)
dhora x = (div x 60, mod x 60)

ehora :: (Int, Int) -> (Int, Int) -> Int
ehora (x1, y1) (x2, y2)
    | bhora (x1, y1) (x2, y2) = (24*60) - (chora (x1, y1) - chora (x2, y2))
    | otherwise = chora (x2, y2) - chora (x1, y1)

fhora :: Int -> (Int, Int) -> (Int, Int)
fhora m (x, y) = dhora (chora (x + div m 60, y + mod m 60))

data Hora = H Int Int deriving (Show,Eq)

aHora :: Hora -> Bool
aHora (H x y) = (0 <= x && x <= 23) && (0 <= y && y <= 59)

bHora :: Hora -> Hora -> Bool
bHora (H x1 y1) (H x2 y2)
    | x1 == x2 = y1 > y2
    | otherwise = x1 > x2

cHora :: Hora -> Int
cHora (H x y) = (x * 60) + y

dHora :: Int -> Hora
dHora x = H (div x 60) (mod x 60)

eHora :: Hora -> Hora -> Int
eHora (H x1 y1) (H x2 y2)
    | bhora (x1, y1) (x2, y2) = (24*60) - (cHora (H x1 y1) - cHora (H x2 y2))
    | otherwise = cHora (H x2 y2) - cHora (H x1 y1)

fHora :: Int -> Hora -> Hora
fHora m (H x y) = dHora (cHora (H (x + div m 60) (y + mod m 60)))

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

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
    | (x == Verde) && (y == Vermelho) = True
    | (x == Vermelho) && (y == Verde) = True
    | otherwise = False

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

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

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

-- poligono :: Figura -> Bool