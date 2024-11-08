module Ficha4 where

import Data.Char

-- Exercicio 1
digitAlpha :: String -> (String, String)
digitAlpha [] = ([], [])
digitAlpha (h:t) | isAlpha h = (h:leters, digits)
                 | isDigit h = (leters, h:digits)
                 | otherwise = (leters, digits)
                 where (leters, digits) = digitAlpha t

-- Exercicio 2
negZerPos :: [Int] -> (Int, Int, Int)
negZerPos [] = (0, 0, 0)
negZerPos (h:t) | h < 0  = (1+neg, zer, pos)
                | h == 0 = (neg, 1+zer, pos)
                | h > 0  = (neg, zer, 1+pos)
                where (neg, zer, pos) = negZerPos t

-- Exercicio 3
divMod' :: Integral a => a -> a -> (a, a)
divMod' 0 _ = (0, 0)
divMod' t d | t >= d    = (1+div, mod)
            | t>d && t>(-d) = (div, t+mod)
            where (div, mod) = divMod' (t-d) d
            --- não está a dar

-- Exercicio 4
fromDigits :: [Int] -> Int
fromDigits is = fst (aux is 0)
    where aux :: [Int] -> Int -> (Int, Int)
          aux [] ac = (0, ac)
          aux (h:t) ac = (h * 10^(ac'+1) + r, ac'+1)
            where (r, ac') = aux t ac

-- Exercicio 5

-- Exercicio 6
fib :: Int -> (Int, Int)
fib 0 = (0,0)
fib 1 = (1,0)
fib n = (act+ant, ant)
    where (ant, act) = fib (n-1)

-- Exercicio 7

-- Exercicio 8
