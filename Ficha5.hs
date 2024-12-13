module Ficha5 where

import Data.List

any' :: (a -> Bool) -> [a] -> Bool
any' pred [] = False
any' pred (x:xs) = pred x || any' pred xs

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x: takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = x:xs

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([], [])
span' pred (x:xs) | pred x = (x:lt, ld)
                | otherwise = ([], x:xs)
                where (lt, ld) = span' pred xs

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' pred term (x:xs) | pred term x = xs
                           | otherwise = x : deleteBy' pred term xs 

insert' :: Ord b => (a -> b) -> a -> [a] -> [a]
insert' f el [] = [el]
insert' f el (x:xs) | f el < f x = el : x : xs
                   | otherwise  = x : insert' f el xs

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' pred (x:xs) = insert' pred x (sortOn' pred xs)

---------------------------------------------------------- EX3
type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK mat = length (nub (map length mat)) == 1  -- 'nub' removes repeated elements

dimMat :: Mat a -> (Int, Int)  -- (#linhas, #colunas)
dimMat mat = (length mat, length (head mat))

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (x:xs) (y:ys) = zipWith' (+) x y : addMat xs ys
addMat _ _ = []

transpose' :: Mat a -> Mat a
transpose' mat | null (head mat) = []
               | otherwise = map head mat : transpose' (map tail mat)

----------------------------------------------------------- EX2

type Polinomio = [Monomio]
type Monomio = (Float, Int)

pol :: Polinomio
pol = [(2,3), (3,4), (5,3), (4,5)]

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\(_, e) -> e == n) p

selgrau' :: Int -> Polinomio -> Polinomio
selgrau' _ [] = []
selgrau' n ((c,e):t) | n == e    = (c,e) : selgrau' n t
                     | otherwise = selgrau' n t

conta :: Int -> Polinomio -> Int
conta n p = foldr (\(_, e) ac -> if e == n then ac+1 else ac) 0 p

conta' :: Int -> Polinomio -> Int
conta' _ [] = 0
conta' n (p:ps) = aux p + conta' n ps
    where aux (_, e) = if e == n then 1 else 0

conta'' :: Int -> Polinomio -> Int
conta'' n p = length (selgrau n p)

grau :: Polinomio -> Int
grau p = foldr (\(_,e) grau -> if e > grau then e else grau) 0 p

grau' :: Polinomio -> Int
grau' [] = 0
grau' (p:ps) | aux p > grau' ps = aux p
             | otherwise        = grau' ps
    where aux (_, e) = e

deriv :: Polinomio -> Polinomio
deriv p = map (\(c, e) -> (c * fromIntegral e, e-1)) (filter (\(_,e) -> e > 0) p)

deriv' :: Polinomio -> Polinomio
deriv' p = foldr (\(c, e) ac -> if e > 0 then (c * fromIntegral e, e - 1) : ac else ac) [] p

deriv'' :: Polinomio -> Polinomio
deriv'' [] = []
deriv'' (p:ps) = aux p ++ deriv' ps
    where aux (c,e) | e > 0     = [(c * fromIntegral e, e-1)]
                    | otherwise = []

calcula :: Float -> Polinomio -> Float
calcula x p = foldr (\(c, e) ac -> ac + c * x^e) 0 p

calcula' :: Float -> Polinomio -> Float
calcula' x p = sum (map (\(c, e) -> c * x^e) p)

mult :: Monomio -> Polinomio -> Polinomio
mult (cm, em) p = foldr (\(c, e) ac -> (cm * c, em + e):ac) [] p

mult' :: Monomio -> Polinomio -> Polinomio
mult' (cm, em) p = map (\(c, e) -> (cm * c, em + e)) p

mult'' :: Monomio -> Polinomio -> Polinomio
mult'' _ [] = []
mult'' (cm, em) ((c,e):ps) = (cm * c, em + e) : mult'' (cm, em) ps