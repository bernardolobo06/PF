-- Exercício 34
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t)
    | h > (t !! pMaior t) = 0
    | otherwise = 1 + pMaior t