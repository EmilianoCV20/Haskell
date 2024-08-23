import Test.QuickCheck

dosVeces :: (a -> a) -> a -> a
dosVeces f x = f (f x)

--Compresion
mapC :: (a -> b) -> [a] -> [b]
mapC f xs = [f x | x <- xs]

--Recursion
mapR :: (a -> b) -> [a] -> [b]
mapR _ [] = []
mapR f (x:xs) = f x : mapR f xs

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

prop_sum_map :: [Int] -> Bool
prop_sum_map xs = sum (map (2*) xs) == 2 * sum xs

--Comprension
filterC :: (a -> Bool) -> [a] -> [a]
filterC p xs = [x | x <- xs, p x]

--Recursion
filterR :: (a -> Bool) -> [a] -> [a]
filterR _ [] = []
filterR p (x:xs) | p x = x : filterR p xs
                                     | otherwise = filterR p xs

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

--Comprension
sumaCuadradosPares' :: [Int] -> Int
sumaCuadradosPares' xs = sum [x^2 | x <- xs, even x]