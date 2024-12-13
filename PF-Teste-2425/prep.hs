module Prep where
import System.Random
import GHC.IO.Buffer (BufferState(ReadBuffer))
import GHC.Base (geWord)

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:ys) = y : posImpares ys

posImpares' :: [b] -> [b]
posImpares' l = map fst (filter (\(_,i) -> odd i) (zip l [1..]))

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _  = True
isPrefixOf _ []  = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isPrefixOf' :: Eq b => [b] -> [b] -> Bool
isPrefixOf' xs ys = length (filter (\(a, b) -> a == b) (zip xs ys)) == length xs

type Mat a = [[a]]

zeros :: (Eq a, Num a) => Mat a -> Int
zeros [] = 0
zeros (l:ls) = zeros' l + zeros ls
    where zeros' [] = 0
          zeros' (x:xs) | x == 0    = 1 + zeros' xs
                        | otherwise = zeros' xs

zeros' :: (Eq a, Num a) => Mat a -> Int
zeros' m  = zeros' (concat m)
     where zeros' [] = 0
           zeros' (x:xs) | x == 0    = 1 + zeros' xs
                         | otherwise = zeros' xs

zeros'' :: (Eq a, Num a) => Mat a -> Int
zeros'' m = foldr (\x ac -> if x == 0 then 1 + ac else ac) 0 (concat m) -- colocar tudo numa lista e contar os elementos iguais a 0

zeros''' :: (Eq a, Num a) => Mat a -> Int
zeros''' m = sum (map (\l -> length (filter (== 0) l)) m)

zeros'''' :: (Eq a, Num a) => Mat a -> Int
zeros'''' m = foldr (+) 0 (map (\l -> length (filter (== 0) l)) m)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (x:xs) (y:ys) = zipWith (+) x y : addMat xs ys
addMat _ _ = []

transpose :: Mat a -> Mat a
transpose [[]] = []
transpose m = map head m : transpose (map tail m)


-- FICHA 2

-- 2a)
dobros :: [Float] -> [Float]
dobros = map (\x -> 2*x)

dobros' :: [Float] -> [Float]
dobros' [] = []
dobros' (x:xs) = (2 * x) : dobros' xs

-- 2b)
numOcorre :: Char -> String -> Int
numOcorre char = foldr (\x ac -> if char == x then ac+1 else ac) 0

numOcorre' :: Char -> String -> Int
numOcorre' char str = aux char str 0
      where aux :: Char -> String -> Int -> Int
            aux _ [] ac = ac
            aux c (x:xs) ac | c == x    = aux c xs (ac+1)
                            | otherwise = aux c xs ac

--2c)
positivos :: [Int] -> Bool
positivos = foldr (\x ac -> x > 0 && ac) True

positivos' :: [Int] -> Bool
positivos' l = null (filter (<= 0) l)

positivos'' :: [Int] -> Bool
positivos'' [] = True
positivos'' (x:xs) = (x > 0) && positivos'' xs

--2d)
soPos :: [Int] -> [Int]
soPos = filter (> 0)

soPos' :: [Int] -> [Int]
soPos' [] = []
soPos' (x:xs) | x > 0     = x : soPos' xs
              | otherwise = soPos' xs

--2e)
somaNeg :: [Int] -> Int
somaNeg l = sum (filter (< 0) l)

somaNeg' :: [Int] -> Int
somaNeg' [] = 0
somaNeg' (x:xs) | x < 0     = x + somaNeg' xs
                | otherwise = somaNeg' xs

--2f)
tresUlt :: [a] -> [a]
tresUlt = foldr (\x ac -> if length ac < 3 then x : ac else ac) []

tresUlt' :: [a] -> [a]
tresUlt' x | length x <= 3 = x
           | otherwise     = aux (reverse x) []
             where aux :: [a] -> [a] -> [a]
                   aux (x:xs) nl | length nl /= 3 = aux xs (x:nl)
                                 | otherwise      = nl

tresUlt'' :: [a] -> [a]
tresUlt'' x = reverse (take 3 (reverse x))

--2g)
segundos :: [(a,b)] -> [b]
segundos = map snd

segundos' :: [(a,b)] -> [b]
segundos' [] = []
segundos' ((_,b):xs) = b : segundos' xs


--2h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros el l = el `elem` (map fst l)

nosPrimeiros' :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros' el l = el `elem` (aux l)
      where aux :: [(a,b)] -> [a]
            aux [] = []
            aux ((a,_):xs) = a : aux xs

--2i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos = foldr (\(a, b, c) (ac1, ac2, ac3) -> (ac1 + a, ac2 + b, ac3 + c)) (0, 0, 0)

sumTriplos' :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos' l = aux l (0, 0, 0)
      where aux :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) -> (a,b,c)
            aux [] (ac1, ac2, ac3) = (ac1, ac2, ac3)
            aux ((a, b, c):xs) (ac1, ac2, ac3) = aux xs (ac1 + a, ac2 + b, ac3 + c)

--4a)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta n = foldr (\(_, e) ac -> if n == e then ac + 1 else ac) 0

--4b)
grau :: Polinomio -> Int
grau = foldr (\(_, e) ac -> max e ac) 0

--4c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n = filter (\(_, e) -> n == e)

--4d)
deriv :: Polinomio -> Polinomio
deriv p = map (\(c, e) -> (c * fromIntegral e, e-1)) (filter (\(_, e) -> e > 0) p)

--4e)
calcula :: Float -> Polinomio -> Float
calcula x = foldr (\(c, e) ac -> ac + (c * x^e)) 0

--4f)
simp :: Polinomio -> Polinomio
simp = filter (\(c, _) -> c /= 0)

--4g)
mult :: Monomio -> Polinomio -> Polinomio
mult (c, e) = map (\(co, ex) -> (co * c, ex + e))

--4h)
normaliza :: Polinomio -> Polinomio
normaliza p = simp (aux (grau p) p)
      where aux :: Int -> Polinomio -> Polinomio
            aux 0 p = [(sum (map fst (filter (\(_, e) -> e == 0) p)), 0)]
            aux g p = (sum (map fst (filter (\(_, e) -> e == g) p)), g) : aux (g-1) p

--4i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

--4j)
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 [] = p1
produto p1 (p:ps) = produto (mult p p1) ps

--4k)
ordena :: Polinomio -> Polinomio
ordena p = reverse (normaliza p)

--4l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = normaliza p1 == normaliza p2


-- Exame 2019/20

--1a)
inits :: [a] -> [[a]]
inits l = reverse (aux (reverse l))
      where aux [] = [[]]
            aux (x:xs) = reverse (x:xs) : aux xs 

--1b)
isPrefixOf'' :: Eq a => [a] -> [a] -> Bool
isPrefixOf'' [] _ = True
isPrefixOf'' _ [] = False
isPrefixOf'' (x:xs) (y:ys) = x == y && isPrefixOf xs ys

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

--2a)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ l r) = folhas l + folhas r

--2b)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a _ _) = [a]
path (x:xs) (Node a l r) | x         = a : path xs r
                         | otherwise = a : path xs l

type Polinomio' = [Coeficiente]
type Coeficiente = Float

--3a)
valor :: Polinomio' -> Float -> Float
valor p x = foldr (\(c, e) ac -> ac + (c * x ^ e)) 0 (zip p [0..])

--3b)
deriv' :: Polinomio' -> Polinomio'
deriv' p = map (\(c, e) -> c * fromIntegral e) (filter (\(_, e) -> e > 0) (zip p [0..]))

--3c)
soma' :: Polinomio' -> Polinomio' -> Polinomio'
soma' [] p = p
soma' p [] = p
soma' (x:xs) (y:ys) = (x+y) : soma' xs ys

--4a) 
quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = []
quebraLinha (x:xs) l = take x l : quebraLinha xs (drop x l)

--4b)
fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (x:xs) c m = fragmentaCols c (take x m) ++ fragmenta xs c (drop x m)

fragmentaCols :: [Int] -> Mat a -> [Mat a]
fragmentaCols [] _ = []
fragmentaCols (x:xs) m = map (take x) m : fragmentaCols xs (map (drop x) m)

--4c)
geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int)
geraMat (l, c) (a, b) = do
      mat <- mapM (\_ -> geraLinha c (a, b)) [1..l]
      return mat
      
      where
            geraLinha 0 _ = return []
            geraLinha n (a, b) = do
                  r <- randomRIO (a, b)
                  rs <- geraLinha (n-1) (a, b)
                  return (r:rs)

-- Teste 2019/20

--1a)
intersect :: Eq a => [a] -> [a] -> [a]
intersect l1 l2 = filter (`elem` l2) l1 

--1b)
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

--2a)
elems :: ConjInt -> [Int]
elems [] = []
elems ((a,b):t) = [a..b] ++ elems t

--2b)
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l = (head l, num l) : geraconj (lst l)
      where num [y] = y
            num (y:ys) = if y == (head ys - 1) then num ys else y
            lst [y] = []
            lst (y:ys) = if y == (head ys - 1) then lst ys else ys

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

--3a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail name mail = map (\(n, lc) -> if name == n then (name, Email mail : lc) else (n, lc))

--3b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((n,c):t) 
    | nome == n = Just (map (\(Email e) -> e) (filter isEmail c))
    | otherwise = verEmails nome t
    where isEmail (Email _) = True
          isEmail _ = False

--3c)
consulta :: [Contacto] -> ([Integer], [String])
consulta =
    foldr (\c (tlfs, emails) ->
        case c of 
            Tlm n -> (n:tlfs, emails)
            Casa n -> (n:tlfs, emails)
            Trab n -> (n:tlfs, emails)
            Email e -> (tlfs, e:emails)
        ) ([],[])




data RTree a = R a [RTree a] deriving (Show, Eq)

--4a)
paths :: RTree a -> [[a]]
paths (R a []) = [[a]]
paths (R a as) = map (a :) (concat (map paths as))

--4b)
unpaths :: Eq a => [[a]] -> RTree a
unpaths [[a]] = R a []
unpaths l = R (head (head l)) (map unpaths (groupByHead ll))
      where ll = map tail l

groupByHead :: Eq a => [[a]] -> [[[a]]]
groupByHead [] = []
groupByHead ((h:t):tt) = ((h:t) : cabecasIguais) : groupByHead cabecasDiferentes
      where cabecasIguais     = filter (\l -> head l == h) tt
            cabecasDiferentes = filter (\l -> head l /= h) tt


-- Exame 23/24

-- 4b)
data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

unDumpLT :: [(a, Int)] -> LTree a
unDumpLT [(x, 1)] = Tip x
unDumpLT (h:t) = fst (unDumpLTAux (h:t))

unDumpLTAux :: [(a, Int)] -> (LTree a, [(a, Int)])
unDumpLTAux ((x,1):t) = (Tip x, t)
unDumpLTAux l = (Fork ltl ltr, map (\(a, b) -> (a, b+1)) rr)
      where (ltl, lr) = unDumpLTAux (map (\(a, b) -> (a, b-1)) l)
            (ltr, rr) = unDumpLTAux lr

--d13177@di.uminho.pt