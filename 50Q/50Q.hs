-- Exercício 34
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t)
    | h > (t !! pMaior t) = 0
    | otherwise = 1 + pMaior t

-------- TPC Pavlo P'Yatkovsky --------
elemIndice :: Eq a => a -> [a] -> [Int]
elemIndice el ls = aux el 0 ls
    where aux x p (h:t)
            | x == h = p : aux x (p+1) t
            | x /= h = aux x (p+1) t
          aux _ _ [] = []