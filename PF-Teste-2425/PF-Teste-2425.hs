module Teste_2425 where

-- Exercício 1
resultadoPossivel :: (Eq a, Num a) => a -> [a] -> Bool
resultadoPossivel el l = el `elem` lista [] l

-- auxiliar
lista :: Num a => [a] -> [a] -> [a]
lista res [] = res
lista [] [x] = [x]
lista [] (x:y:t) = lista [x+y, x-y] t
lista res (x:t)  = lista (aux res x) t
    where aux :: Num a => [a] -> a -> [a]
          aux []    _  = []
          aux (x:y) el = [x+el, x-el] ++ aux y el

-- Exercício 2
deleteMin :: Ord a => [a] -> [a]
deleteMin l = snd (minRem l) 

-- auxiliar
minRem :: Ord a => [a] -> (a, [a])
minRem l = aux (l, [])
    where  aux :: Ord a => ([a], [a]) -> (a, [a])
           aux ([], x:xs) = (x, xs)
           aux (x:xs, []) = aux (xs, [x])
           aux (x:xs, y:ys) | y < x     = aux (xs, y:ys ++ [x])
                            | y > x     = aux (xs, x:y:ys)
                            | otherwise = aux (xs, y:ys)

-- Exercício 3
data LTree a = Leaf a |
               Fork (LTree a) (LTree a)
 
-- a)
instance Show a => Show (LTree a) where
    show :: Show a => LTree a -> String
    show (Leaf a)   = show a
    show (Fork l r) = "(" ++ show l ++ " <|> " ++ show r ++ ")"

-- b)
procuraTodos :: Eq a => a -> LTree (a, b) -> Maybe [b]
procuraTodos x lt | null (procura x lt) = Nothing
                  | otherwise           = Just (procura x lt)  

-- auxiliar
leafs :: LTree (a, b) -> [(a, b)]
leafs (Leaf x)   = [x]
leafs (Fork l r) = leafs l ++ leafs r

-- auxiliar
procura :: Eq a => a -> LTree (a, b) -> [b]
procura x lt = map snd (filter (\(a, b) -> a == x) (leafs lt))