module Ficha3 where

--escrever documentação haddock

data Hora = H Int Int deriving Show  -- Exercício 1
type Etapa = (Hora, Hora)
type Viagem = [Etapa]

horaValida :: Hora -> Bool  
horaValida (H h m) = (0 <= h && h <= 23) && (0 <= m && m <= 59)

-- True if h1 > h2
-- True if h1 == h2 and m1 > m2
-- False otherwise
maiorHora :: Hora -> Hora -> Bool
maiorHora (H h1 m1) (H h2 m2)
    | h1 == h2 = m1 > m2
    | otherwise = h1 > h2

etapaValida :: Etapa -> Bool
etapaValida (init, end) = horaValida init && horaValida end && maiorHora end init

viagemValida :: Viagem -> Bool
viagemValida [e] = etapaValida e
viagemValida (e1:e2:t) = etapaValida e1 && maiorHora (fst e2) (snd e1) && viagemValida (e2:t)  -- && etapaValida e2 was removed cause it is redundant since this is verified once the recursive call begins.
