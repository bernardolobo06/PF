module Ficha7 where
import Data.Bits (Bits(xor))


data ExpInt = Const     Int
            | Simetrico ExpInt
            | Mais      ExpInt ExpInt
            | Menos     ExpInt ExpInt
            | Mult      ExpInt ExpInt
            deriving Show


--"3+4*2"
exp1 :: ExpInt
exp1 = Mais (Const 3) 
            (Mult (Const 4) (Const 2))

--"(3+4)*2"
exp2 :: ExpInt
exp2 = Mult (Mais (Const 3) (Const 4)) 
            (Const 2)


calcula :: ExpInt -> Int
calcula (Const x)     = x
calcula (Simetrico e) = - calcula e
calcula (Mais e1 e2)  = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2)  = calcula e1 * calcula e2

infixa :: ExpInt -> String
infixa (Const x)     = show x
infixa (Simetrico e) = "-(" ++ infixa e ++ ")"
infixa (Mais e1 e2)  = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2)  = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x)     = show x
posfixa (Simetrico e) = "-(" ++ posfixa e ++ ")"
posfixa (Mais e1 e2)  = posfixa e1 ++ posfixa e2 ++ "+"
posfixa (Menos e1 e2) = posfixa e1 ++ posfixa e2 ++ "-"
posfixa (Mult e1 e2)  = posfixa e1 ++ posfixa e2 ++ "*"


data RTree a = R a [RTree a] deriving Show


rT :: RTree Int
rT = R 5 [R 2 [R 4 [],
               R 8 [R 5 []]], 
          R 3 [],
          R 7 [R 11 []]]


soma :: Num a => RTree a -> a
soma (R v []) = v
soma (R v l)  = v + sum (map soma l) --sum [v1, v2 ...]

altura :: RTree a -> Int
altura (R _ []) = 1
altura (R _ l) = 1 + maximum (map altura l) --maximum /= max | maximum is used on lists | max is used on 2 args

prune :: Int -> RTree a -> RTree a
prune 1 (R v _)  = R v []
prune i (R v l)  = R v (map (prune (i-1)) l)