module Ficha5 where

import Data.List
import Type.Reflection (SomeTypeRep(SomeTypeRep))
import Data.Type.Coercion (trans)

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




------------------ Teste
matriz :: Mat Int
matriz = [[1,2,3],
          [0,4,5],
          [0,0,6]]

test = transpose' matriz