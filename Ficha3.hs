module Ficha3 where

import Ficha1 (Hora (..), aHora, bHora, 
               Ponto (..), posx, posy, distancia, 
               Figura (..), area) --obrigatório importar os construtores do data type. ao usar (..) todos os construtores são importados

--escrever documentação haddock

-- Exercício 1
type Etapa = (Hora, Hora)
type Viagem = [Etapa]

etapaValida :: Etapa -> Bool
etapaValida (init, end) = aHora init && aHora end && bHora end init

viagemValida :: Viagem -> Bool
viagemValida [e] = etapaValida e
viagemValida (e1:e2:t) = etapaValida e1 && bHora (fst e2) (snd e1) && viagemValida (e2:t)  -- && etapaValida e2 was removed cause it is redundant since this is verified once the recursive call begins.

partidaChegada :: Viagem -> (Hora, Hora)
partidaChegada viagem = (fst (head viagem), snd (last viagem))

tempoEfetivo :: Viagem -> Int
tempoEfetivo [] = 0
tempoEfetivo (e:t) = tempoEtapa e + tempoEfetivo t

----------------- auxiliar function --------------------
tempoEtapa :: Etapa -> Int
tempoEtapa (H h1 m1, H h2 m2) = (h2*60+m2) - (h1*60+m1)
--------------------------------------------------------

tempoEspera :: Viagem -> Hora
tempoEspera viagem = let total = tempoTotal viagem
                         efetivo = tempoEfetivo viagem
                         dif = total - efetivo
                     in H (dif `div` 60) (dif `mod` 60)

tempoTotal :: Viagem -> Int
tempoTotal viagem = let (init, end) = partidaChegada viagem
                    in tempoEtapa (init, end)

-- Exercício 2
type Poligonal = [Ponto]

compLnPol :: Poligonal -> Double
compLnPol [p1,p2] = distancia p1 p2
compLnPol (h1:h2:t) = distancia h1 h2 + compLnPol (h2:t)

closedLn :: Poligonal -> Bool
closedLn ls = length ls > 3 && head ls == last ls

-- imaginemos que a Poligonal é definida por 7 pontos p1 a p7 (p1 = p7)
-- dividindo o poligonal em vários triângulos, obtemos a sua área pela soma das áreas dos triângulos
-- obtém-se, portanto: p1, p2, p3 | p1, p3, p4 | p1, p4, p5 | p1, p5, p6
-- o primeiro ponto é igual ao último por definição
triangula :: Poligonal -> [Figura]
triangula [p1, p2, p3, p4] = [Triangulo p1 p2 p3]
triangula (p1:p2:p3:t) = Triangulo p1 p2 p3 : triangula (p2:p3:t)
triangula _ = []

areaPol :: Poligonal -> Double
areaPol plg = aux ls
    where ls = triangula plg
          aux [] = 0
          aux (x:xs) = area x + aux xs

-- recebe Cartesiano e Polar e devolve conversão em Cartesiano
mover :: Poligonal -> Ponto -> Poligonal
mover plg pnt = aux plg (dx, dy)
    where (dx, dy) = cDelta (head plg) pnt
          cDelta p1 p2 = (posx p2-posx p1, posy p2- posy p1)
          aux [] _ = []
          aux (p1:t) (dx, dy) = Cartesiano (posx p1 + dx) (posy p1 + dy) : aux t (dx, dy)

zoom :: Double -> Poligonal -> Poligonal
zoom _ [] = []
zoom esc (h:t) = h : aux h t
    where aux _ [] = []
          aux (Cartesiano x0 y0) ((Cartesiano x y):t) =
            let dx = (x - x0) * esc
                dy = (y - y0) * esc
            in Cartesiano (x0 + dx) (y0 + dy) : aux (Cartesiano x0 y0) t























-- Exercício 3
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

addEmail :: Nome -> String -> Agenda -> Agenda
addEmail nome email [] = [(nome, [Email email])]
addEmail nome email ((n, c):cs)
    | nome == n = (n, Email email : c) : cs
    | otherwise = (n, c) : addEmail nome email cs

showEmail :: [Contacto] -> [String]
showEmail [] = []
showEmail ((Email x) : cs) = x : showEmail cs
showEmail (_ : cs) = showEmail cs

-------------auxiliar function---------------
getEmail :: Nome -> Agenda -> Maybe [String]
getEmail nome [] = Nothing
getEmail nome ((n, c):cs)
    | nome == n = Just (showEmail c)
    | otherwise = getEmail nome cs
---------------------------------------------









------- TPC Pavlo P'Yatkovsky
elemIndice :: Eq a => a -> [a] -> [Int]
elemIndice el ls = aux el 0 ls
    where aux x p (h:t)
            | x == h = p : aux x (p+1) t
            | x /= h = aux x (p+1) t
          aux _ _ [] = []