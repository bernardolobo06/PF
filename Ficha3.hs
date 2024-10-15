module Ficha3 where

--escrever documentação haddock

data Hora = H Int Int deriving Show  -- Exercício 1
type Etapa = (Hora, Hora)
type Viagem = [Etapa]

------------------------------------------------ auxiliar functions copied from Ficha1 -------------------------------------------------
horaValida :: Hora -> Bool                                                                                                          -- |
horaValida (H h m) = (0 <= h && h <= 23) && (0 <= m && m <= 59)                                                                     -- |
                                                                                                                                    -- |
-- True if h1 > h2                                                                                                                  -- |
-- True if h1 == h2 and m1 > m2                                                                                                     -- |
-- False otherwise                                                                                                                  -- |
maiorHora :: Hora -> Hora -> Bool                                                                                                   -- |
maiorHora (H h1 m1) (H h2 m2)                                                                                                       -- |
    | h1 == h2 = m1 > m2                                                                                                            -- |
    | otherwise = h1 > h2                                                                                                           -- |
----------------------------------------------------------------------------------------------------------------------------------------

etapaValida :: Etapa -> Bool
etapaValida (init, end) = horaValida init && horaValida end && maiorHora end init

viagemValida :: Viagem -> Bool
viagemValida [e] = etapaValida e
viagemValida (e1:e2:t) = etapaValida e1 && maiorHora (fst e2) (snd e1) && viagemValida (e2:t)  -- && etapaValida e2 was removed cause it is redundant since this is verified once the recursive call begins.

partidaChegada :: Viagem -> (Hora, Hora)
partidaChegada viagem = (fst (head viagem), snd (last viagem))

-- auxiliar function
tempoEtapa :: Etapa -> Int
tempoEtapa (H h1 m1, H h2 m2) = (h2*60+m2) - (h1*60+m1)

tempoEfetivo :: Viagem -> Int
tempoEfetivo [] = 0
tempoEfetivo (e:t) = tempoEtapa e + tempoEfetivo t

tempoTotal :: Viagem -> Int
tempoTotal viagem = let (init, end) = partidaChegada viagem
                    in tempoEtapa (init, end)

tempoEspera :: Viagem -> Hora
tempoEspera viagem = let total = tempoTotal viagem
                         efetivo = tempoEfetivo viagem
                         dif = total - efetivo
                     in H (dif `div` 60) (dif `mod` 60)

-- Exercício 3
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

addEmail :: Nome -> String -> Agenda -> Agenda
addEmail nome email [] = [(nome, [Email email])]
addEmail nome email ((n, c):cs)
    | nome == n = (n, Email email : c) : cs
    | otherwise = (n, c) : addEmail nome email cs

emails :: [Contacto] -> [String]
emails [] = []
emails ((Email x) : cs) = x : emails cs
emails (_ : cs) = emails cs

getEmail :: Nome -> Agenda -> Maybe [String]
getEmail nome [] = Nothing
getEmail nome ((n, c):cs)
    | nome == n = Just (emails c)
    | otherwise = getEmail nome cs