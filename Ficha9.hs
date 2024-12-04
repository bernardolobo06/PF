module Ficha9 where

import System.Random
import Data.Char (digitToInt)

-- gera uma data aleatória
dataA :: IO (Int, Int, Int)
dataA = do
    dd   <- randomRIO (1, 31)
    mm   <- randomRIO (1, 12)
    aaaa <- randomRIO (2000, 2024)
    return (dd, mm, aaaa)

-- gera uma lista de n inteiros aleatórios entre a e b
geraLista :: Int -> (Int, Int) -> IO [Int]
geraLista 0 _      = return []
geraLista n (a, b) = do val   <- randomRIO (a, b)
                        lista <- geraLista (n-1) (a, b)
                        return (val:lista)

-- gera uma matriz de dimensão n por m com valores aleatórios entre a e b
type Mat a = [[a]]
geraMatriz :: (Int, Int) -> (Int, Int) -> IO (Mat Int)
geraMatriz (0, _) _      = return []
geraMatriz (n, m) (a, b) = do linha  <- geraLista m (a, b)
                              matriz <- geraMatriz (n-1, m) (a, b)
                              return (linha:matriz)

-- factorial interativo
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

factIO :: IO ()
factIO = do putStr "Introduza um número: "
            v <- getLine
            i <- readIO v
            let r = fact i
            putStrLn ("O factorial do número " ++ v ++ " é: " ++ show r)


-- BINGO
bingo :: IO ()
bingo = do putStrLn "Começou um novo jogo!"
           l <- jogar []
           putStrLn ("O jogo terminou com a ordem: " ++ show l)

jogar :: [Int] -> IO [Int]
jogar l | length l == 90 = return l
        | otherwise      = do n <- randomRIO (1, 90)
                              if n `elem` l
                              then jogar l
                              else do jogar (n:l)

{-
TPC
Começar com a lista [1..90] e gerar aleatoriamente um índice.
Depois remover da lista, e diminuir o indice -1
-}


-- MASTERMIND
mastermind :: IO ()
mastermind = do chave <- geraChave
                putStrLn "Wlecome to Mastermind!"
                putStrLn "Introduza uma chave (4 digitos):"
                jogarMM chave

geraChave :: IO (Int, Int, Int, Int)
geraChave = do a <- randomRIO (0, 9)
               b <- randomRIO (0, 9)
               c <- randomRIO (0, 9)
               d <- randomRIO (0, 9)
               return (a, b, c, d)

jogarMM :: (Int, Int, Int, Int) -> IO ()
jogarMM chave = do jogada <- getLine
                   certos <- iguais chave (converteChave jogada)
                   if certos == 4
                   then putStrLn "Encontrou a chave secreta!"
                   else do putStrLn ("Acertou em: " ++ show certos ++ " posições.")
                           jogarMM chave

iguais :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> IO Int
iguais (a, b, c, d) (x, y, z, w) = return (length (filter (== True) [a == x, b == y, c == z, d == w]))

converteChave :: String -> (Int, Int, Int, Int)
converteChave [a, b, c, d] = (digitToInt a, digitToInt b, digitToInt c, digitToInt d)