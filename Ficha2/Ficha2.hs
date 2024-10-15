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

---passou para a alinea h)---


-- auxiliar function for h)
insere :: Monomio -> Polinomio -> Polinomio
insere (c,e) [] = [(c,e)]
insere (c,e) ((cp,ep):t) = if e == ep then (c+cp, e) : t else (cp,ep) : insere (c,e) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,e):t) = insere (c,e) (normaliza t)