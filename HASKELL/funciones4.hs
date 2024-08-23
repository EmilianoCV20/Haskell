inserta :: Ord a => a -> [a] -> [a]
inserta e []                            = [e]
inserta e (x:xs) | e <= x = e : (x:xs)
                                       | otherwise = x : inserta e xs

ordena_por_insercion :: Ord a => [a] -> [a]
ordena_por_insercion [] = []
ordena_por_insercion (x:xs) = inserta x (ordena_por_insercion xs)

zip1 :: [a] -> [b] -> [(a, b)]
zip1 []              _           = []
zip1 _               []          = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

{-
ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena (x:xs) = 
        (ordena menores) ++ [x] ++ (ordena mayores)
         where menores = [a | a <- xs, a <= x]
                         mayores = [b | b <- xs, b > x]
-}

par :: Int -> Bool
par 0 = True
par n = impar (n-1)

impar :: Int -> Bool
impar 0 = False
impar n = par (n-1)

pares :: [a] -> [a]
pares [] = []
pares (x:xs) = x : impares xs

impares :: [a] -> [a]
impares [] = []
impares (_:xs) = pares xs

product0 :: [Int] -> Int
product0 [] = 1
product0 (n:ns) = n * product0 ns

product1 :: Num a => [a] -> a
product1 = foldr (*) 1

drop0 :: Int -> [a] -> [a]
drop0 0 [] = []
drop0 0 (x:xs) = x:xs
drop0 n [] = []
drop0 n (x:xs) = drop0 n xs

drop1 :: Integral b => b -> [a] -> [a]
drop1 0 xs = xs
drop1 n [] = []
drop1 n (_:xs) = drop1 n xs
