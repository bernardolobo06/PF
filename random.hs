module Random where

import System.Random

rand :: IO ()
rand = do r <- randomRIO (1, 20)
          putStrLn "Adivinhe o número: "
          x <- play r 1
          putStrLn $ "Acertou no número " ++ show r ++ " em " ++ show x ++ " tentativas!"

play :: Int -> Int -> IO Int
play r n = do s <- readLn
              if s == r then return n
              else do if s > r then do putStrLn "Esse número é maior."
                                       play r (n + 1)
                               else do putStrLn "Esse número é menor."
                                       play r (n + 1)