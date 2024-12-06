module Prep where

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:ys) = y : posImpares ys

posImpares' :: [b] -> [b]
posImpares' l = map fst (filter (\(_,i) -> odd i) (zip l [1..]))

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _  = True
isPrefixOf _ []  = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isPrefixOf' :: Eq b => [b] -> [b] -> Bool
isPrefixOf' xs ys = length (filter (\(a, b) -> a == b) (zip xs ys)) == length xs

type Mat a = [[a]]

zeros :: (Eq a, Num a) => Mat a -> Int
zeros [] = 0
zeros (l:ls) = zeros' l + zeros ls
    where zeros' [] = 0
          zeros' (x:xs) | x == 0    = 1 + zeros' xs
                        | otherwise = zeros' xs

zeros' :: (Eq a, Num a) => Mat a -> Int
zeros' m  = zeros' (concat m)
     where zeros' [] = 0
           zeros' (x:xs) | x == 0    = 1 + zeros' xs
                         | otherwise = zeros' xs

zeros'' :: (Eq a, Num a) => Mat a -> Int
zeros'' m = foldr (\x ac -> if x == 0 then 1 + ac else ac) 0 (concat m) -- colocar tudo numa lista e contar os elementos iguais a 0

zeros''' :: (Eq a, Num a) => Mat a -> Int
zeros''' m = sum (map (\l -> length (filter (== 0) l)) m)

zeros'''' :: (Eq a, Num a) => Mat a -> Int
zeros'''' m = foldr (+) 0 (map (\l -> length (filter (== 0) l)) m)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (x:xs) (y:ys) = zipWith (+) x y : addMat xs ys
addMat _ _ = []

transpose :: Mat a -> Mat a
transpose [[]] = []
transpose m = map head m : transpose (map tail m)