module Ficha6 where

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

testBTree = Node 5 (Node 3 Empty Empty)
                   (Node 7 (Node 6 Empty Empty)
                           (Node 9 (Node 8 Empty Empty) Empty))

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ l r) = 1 + max (altura l) (altura r)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ l r) = 1 + contaNodos l + contaNodos r

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ l r) = folhas l + folhas r

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune i (Node a l r) = Node a (prune (i-1) l) (prune (i-1) r)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a _ _) = [a]
path (h:t) (Node a l r) | h         = a : path t r
                        | otherwise = a : path t l

mirror :: BTree a -> BTree a
mirror 



-- Exercício Extra: Converter lista de inteiros em BTree de inteiros.