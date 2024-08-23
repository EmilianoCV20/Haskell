--por Composicion
esDigito :: Char ->Bool
esDigito c = c >= '0' && c <= '9'

par :: (Integral a) => a -> Bool
par n = n `rem` 2 == 0

divideEn :: Int -> [a] -> ([a],[a])
divideEn n xs = (take n xs, drop n xs)

--Con condicionales
absoluto :: Int -> Int
absoluto n = if n >= 0 then n else -n

signoNum :: Int -> Int
signoNum n = if n < 0 then (-1) else
     if n == 0 then 0 else 1

absoluto2 :: Int -> Int
absoluto2 n | n >= 0 = n
    | otherwise = -n

signoNum2 :: Int -> Int
signoNum2 n | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

test1 :: [Char ] -> Bool
test1 ['a',_,_] = True
test1 _ = False

test2 :: [Char ] -> Bool
test2 ('a':_) = True
test2 _ = False

nulo :: [a] -> Bool
nulo [] = True
nulo (_:_) = False

cabeza :: [a] -> a
cabeza (x:_) = x

cola :: [a] -> [a]
cola (_:xs) = xs

predecesor :: Int -> Int
predecesor 0 = 0
predecesor (n+1) = n
-- :set -XHaskell98

impares n = map f [0..n-1]
    where f x = 2*x+1

maxTres x y z = max (max x y) z