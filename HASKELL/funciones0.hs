-- cuadrado :: Int -> Int
cuadrado n = n * n

-- max1 :: Int -> Int -> Int
max1 n m
    | n >= m = n
    | n < m = m

--sumCuad :: Int -> Int
sumCuad n
    | n==0 = 0
    | n > 0 = cuadrado n + sumCuad (n-1)

suma :: (Int,Int) -> Int
suma (x,y) = x+y

deCeroA :: Int -> [Int]
deCeroA n = [0..n]

suma' :: Int -> (Int -> Int)
suma' x y = x+y