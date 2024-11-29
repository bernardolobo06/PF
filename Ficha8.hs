module Ficha8 where

data Frac = F Integer Integer

-- Exercício 1
mdc :: Integer -> Integer -> Integer
mdc x 0 = abs x
mdc x y = mdc y (x `mod` y)
-- mdc x y == mdc (x+y) y == mdc x (y+x)

normaliza :: Frac -> Frac
normaliza (F n d) = F (signal * (n `div` mdc')) (abs (d `div` mdc'))
    where mdc' = mdc n d
          signal = signum d


instance Eq Frac where
    (==) = fracEq

fracEq :: Frac -> Frac -> Bool
fracEq f1 f2 = n1 == n2 && d1 == d2
    where (F n1 d1) = normaliza f1
          (F n2 d2) = normaliza f2


instance Ord Frac where
    (<=) = fracOrd

fracOrd :: Frac -> Frac -> Bool
fracOrd f1 f2 = n1 * d2 <= n2 * d1
    where (F n1 d1) = normaliza f1
          (F n2 d2) = normaliza f2


instance Show Frac where
    show = fracShow

fracShow :: Frac -> String
fracShow (F n d) = "(" ++ show n ++ "/" ++ show d ++ ")"


instance Num Frac where
    (+) = somaFrac
    (*) = multFrac
    abs = absFrac
    signum = sigFrac
    fromInteger = frIntFrac
    (-) = subFrac

somaFrac :: Frac -> Frac -> Frac
somaFrac f1 f2 = normaliza (F (n1 * d2 + n2 * d1) (d1 * d2))
    where (F n1 d1) = f1
          (F n2 d2) = f2

multFrac :: Frac -> Frac -> Frac
multFrac f1 f2 = normaliza (F (n1 * n2) (d1 * d2))
    where (F n1 d1) = f1
          (F n2 d2) = f2

absFrac :: Frac -> Frac
absFrac (F n d) = F (abs n) (abs d)

sigFrac :: Frac -> Frac
sigFrac f = F (signum n') 1
    where (F n' _) = normaliza f

frIntFrac :: Integer -> Frac
frIntFrac num = F num 1

subFrac :: Frac -> Frac -> Frac
subFrac f1 f2 = normaliza (F (n1 * d2 - n2 * d1) (d1 * d2))
    where (F n1 d1) = f1
          (F n2 d2) = f2


fltrFrac :: Frac -> [Frac] -> [Frac]
fltrFrac (F n d) = filter (> f')
    where f' = F (n*2) d