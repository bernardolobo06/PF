module Ficha2 where

import Data.Char

---Exercício 2---
dobros :: [Float] -> [Float]
dobros [y] = [y*2]
dobros (y:ys) = (y*2) : dobros ys

numOcorre :: Char -> String -> Int
numOcorre char [] = 0
numOcorre char (y:ys)
    | char == y = 1 + numOcorre char ys
    | char /= y = 0 + numOcorre char ys

positivos :: [Int] -> Bool
positivos [i] = i > 0
positivos (y:ys) =  y > 0 && positivos ys

soPos :: [Int] -> [Int]
soPos [] = []
soPos (y:ys)
    | y > 0 = y : soPos ys
    | otherwise = soPos ys

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (y:ys)
    | y < 0 = y + somaNeg ys
    | otherwise = somaNeg ys

tresUlt :: [a] -> [a]
tresUlt (y:ys)
    | length (y:ys) <= 3 = y:ys
    | otherwise = tresUlt ys

segundos :: [(a,b)] -> [b]
segundos [(a,b)] = [b]
segundos ((a,b):ys) = b : segundos ys

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros e [(a,b)] = e == a
nosPrimeiros e ((a,b):ys) = e == a || nosPrimeiros e ys

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0, 0, 0)
sumTriplos ((a, b, c):t) = (a + x, b + y, c + z)
    where (x, y, z) = sumTriplos t


---Exercicio 3---
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)
    | isDigit h = h : soDigitos t
    | otherwise = soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)
    | isLower h = 1 + minusculas t
    | otherwise = minusculas t

nums :: String -> [Int]
nums [] = []
nums (h:t)
    | isDigit h = digitToInt h : nums t
    | otherwise = nums t


---Exercicio 4---
type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((_,e):t)
    | e == n = 1 + conta n t
    | otherwise = conta n t

grau :: Polinomio -> Int
grau [] = 0
grau ((_,e):t) = max e (grau t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((b, e):t)
    | e == n = (b, e) : selgrau n t
    | otherwise = selgrau n t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((b, e):t) = (b*fromIntegral e, e-1) : deriv t

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula n ((b, e):t) = (b*(n^e)) + calcula n t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((b, e):t)
    | e == 0 = simp t
    | otherwise = (b, e) : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a, x) ((b, e):t) = (a*b, x+e) : mult (a, x) t

normaliza :: Polinomio -> Polinomio           -------------------------------------------------------------------------------------------------
normaliza [] = []
normaliza ((c,e):t) = insere (c,e) (normaliza t)

--auxiliar function for normaliza
insere :: Monomio -> Polinomio -> Polinomio
insere (c,e) [] = [(c,e)]
insere (c,e) ((cp,ep):t) = if e == ep then (c+cp, e) : t else (cp,ep) : insere (c,e) t -----------------------------------------REVER ALINEA H)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p2 = normaliza (mult h p2 ++ produto t p2)

ordena :: Polinomio -> Polinomio             ---------------------------------------------------------------------------------------------------
ordena [] = []
ordena pol = insert (head (normaliza pol)) (ordena (tail (normaliza pol)))

--auxiliar function
insert :: Monomio -> Polinomio -> Polinomio
insert (c,e) [] = [(c,e)]
insert (c,e) ((cp,ep):ys)
    | e <= ep = (c,e):(cp,ep):ys
    | otherwise = (cp,ep) : insert (c,e) ys         ------------------------------------------------------------------------------REVER INSERTION SORT

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)