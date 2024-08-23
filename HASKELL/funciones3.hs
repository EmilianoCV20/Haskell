concatenar :: [[a]] -> [a]
concatenar xss = [x | xs <- xss, x <- xs]

primerosL :: [(a, b)] -> [a]
primerosL ps = [x | (x,_) <- ps]

longitud :: [a] -> Int
longitud xs = sum [1 | _ <- xs]

factoresDe :: Int -> [Int]
factoresDe n = [x | x <- [1..n], n `mod` x == 0]

esPrimo :: Int -> Bool
esPrimo n = factoresDe n == [1, n]

primos :: Int -> [Int]
primos n = [x | x <- [2..n], esPrimo x]

buscar :: Eq a => a -> [(a, b)] -> [b]
buscar c t = [v | (c', v) <- t, c' == c]

adyacente :: [a] -> [(a, a)]
adyacente xs = zip xs (tail xs)

ordenada :: Ord a => [a] -> Bool
ordenada xs = and [x <= y | (x,y) <- adyacente xs]

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs =
    [i | (x',i) <- zip xs [0..n], x == x']
    where n = length xs - 1

minusculas :: String -> String
minusculas xs = [x | x <- xs, elem x ['a'..'z']]

ocurrencias :: Char -> String -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']

