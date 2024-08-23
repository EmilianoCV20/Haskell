{-
TAREA 23 LISTAS INFINITAS

EQUIPO:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

Objetivo: Tarea23 , Ejercicios.
    Reafirmar y poner en práctica el conocimiento sobre Ia definición de funciones 
    y su uso , así como los tipos de datos.
-}
{-
Ejercicio 7.15.1. Definir la constante
    hamming :: [Int]
tal que hamming es la sucesión de Hamming. Por ejemplo,
    take 12 hamming == [1,2,3,4,5,6,8,9,10,12,15,16]
-}
hamming :: [Int]
hamming = 1 : mezcla3 [2*i | i <- hamming]
                                                   [3*i | i <- hamming]
                                                   [5*i | i <- hamming]

mezcla3 :: [Int] -> [Int] -> [Int] -> [Int]
mezcla3 xs ys zs = mezcla2 xs (mezcla2 ys zs)

mezcla2 :: [Int] -> [Int] -> [Int]
mezcla2 p@(x:xs) q@(y:ys) | x < y              = x:mezcla2 xs q
                                                                 | x > y              = y:mezcla2 p ys
                                                                 | otherwise = x:mezcla2 xs ys
mezcla2 []      ys                                                        = ys
mezcla2 xs    []                                                          = xs


{-
Ejercicio 7.15.2. Definir la función
    divisoresEn :: Int -> [Int] -> Bool
tal que (divisoresEn x ys) se verifica si x puede expresarse como un producto de potencias
de elementos de ys. Por ejemplo,
    divisoresEn 12 [2,3,5] == True
    divisoresEn 14 [2,3,5] == False
-}
divisoresEn :: Int -> [Int] -> Bool
divisoresEn 1  _                                           = True
divisoresEn x  []                                          = False
divisoresEn x (y:ys) | mod x y == 0 = divisoresEn (div x y) (y:ys)
                                                  | otherwise = divisoresEn x ys

{-
Ejercicio 7.15.3. Definir, usando divisoresEn, la constante
    hamming' :: [Int]
tal que hamming’ es la sucesión de Hamming. Por ejemplo,
    take 12 hamming' == [1,2,3,4,5,6,8,9,10,12,15,16]
-}
hamming' :: [Int]
hamming' = [x | x <- [1..], divisoresEn x [2,3,5]]

{-
Ejercicio 7.15.4. Definir la función
    cantidadHammingMenores :: Int -> Int
tal que (cantidadHammingMenores x) es la cantidad de números de Hamming menores que x.
Por ejemplo,
    cantidadHammingMenores 6 == 5
    cantidadHammingMenores 7 == 6
    cantidadHammingMenores 8 == 6   
-}
cantidadHammingMenores :: Int -> Int
cantidadHammingMenores x = length (takeWhile (<x) hamming')

{-
Ejercicio 7.15.5. Definir la función
    siguienteHamming :: Int -> Int
tal que (siguienteHamming x) es el menor número de la sucesión de Hamming mayor que x.
Por ejemplo,
    siguienteHamming 6 == 8
    siguienteHamming 21 == 24
-}
siguienteHamming :: Int -> Int
siguienteHamming x = head (dropWhile (<=x) hamming')

{-
Ejercicio 7.15.6. Definir la función
    huecoHamming :: Int -> [(Int,Int)]
tal que (huecoHamming n) es la lista de pares de números consecutivos en la sucesión
de Hamming cuya distancia es mayor que n. Por ejemplo,
    take 4 (huecoHamming 2) == [(12,15),(20,24),(27,30),(32,36)]
    take 3 (huecoHamming 2) == [(12,15),(20,24),(27,30)]
    take 2 (huecoHamming 3) == [(20,24),(32,36)]
    head (huecoHamming 10) == (108,120)
    head (huecoHamming 1000) == (34992,36000)
-}
huecoHamming :: Int -> [(Int,Int)]
huecoHamming n = [(x,y) | x <- hamming',
                                                             let y = siguienteHamming x,
                                                             y-x > n]

{-
Ejercicio 7.15.7. Comprobar con QuickCheck que para todo n, existen pares de números
 consecutivos en la sucesión de Hamming cuya distancia es mayor o igual que n.
-}
prop_Hamming :: Int -> Bool
prop_Hamming n = huecoHamming n' /= []
                                           where n' = abs n

{-
Ejercicio 7.16.1 (Problema 10 del Proyecto Euler). Definir la función
    sumaPrimoMenores :: Int -> Int
tal que (sumaPrimoMenores n) es la suma de los primos menores que n. Por ejemplo,
    sumaPrimoMenores 10 == 17
-}
{- sumaPrimoMenores :: Int -> Int
sumaPrimoMenores n = sumaMenores n primos 0
             where sumaMenores n (x:xs) a | n <= x = a
                                                                                   | otherwise = sumaMenores n xs (a+x) -}

{-
Ejercicio 7.17.1 (Problema 12 del Proyecto Euler). La sucesión de los números triangulares
se obtiene sumando los números naturales. Así, el 7o número triangular es
    1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
Los primeros 10 números triangulares son
    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, . . .
Los divisores de los primeros 7 números triangulares son:
    1 : 1
    3 : 1, 3
    6 : 1, 2, 3, 6
    10 : 1, 2, 5, 10
    15 : 1, 3, 5, 15
    21 : 1, 3, 7, 21
    28 : 1, 2, 4, 7, 14, 28
Como se puede observar, 28 es el menor número triangular con más de 5 divisores.
Definir la función
    euler12 :: Int -> Integer
tal que (euler12 n) es el menor número triangular con más de n divisores. Por ejemplo,
    euler12 5 == 28
-}
euler12 :: Int -> Integer
euler12 n = head [x | x <- triangulares, nDivisores x > n]

triangulares :: [Integer]
triangulares = 1:[x+y | (x,y) <- zip [2..] triangulares]

divisores :: Integer -> [Integer]
divisores x = [y | y <- [1..x], mod x y == 0]

nDivisores :: Integer -> Int
nDivisores x = length (divisores x)

{-
Ejercicio 7.18.1. Definir la función
    primosEquivalentes :: Int -> [[Integer]]
tal que (primosEquivalentes n) es la lista de las sucesiones de n números primos 
consecutivos con la media de sus dígitos iguales. Por ejemplo,
    take 2 (primosEquivalentes 2) == [[523,541],[1069,1087]]
    head (primosEquivalentes 3) == [22193,22229,22247]
-}


{-
Ejercicio 7.19.1. Definir la función
    perteneceRango :: Int -> (Int -> Int) -> Bool
tal que (perteneceRango x f) se verifica si x pertenece al rango de la función f, suponiendo
que f es una función creciente cuyo dominio es el conjunto de los números naturales.
Por ejemplo,
    perteneceRango 5 (\x -> 2*x+1) == True
    perteneceRango 1234 (\x -> 2*x+1) == False
-}


{-
Ejercicio 7.20.1. Definir, por recursión, la función
    paresOrdenados :: [a] -> [(a,a)]
tal que (paresOrdenados xs) es la lista de todos los pares de elementos (x,y) de xs, tales que
x ocurren en xs antes que y. Por ejemplo,
    paresOrdenados [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
    paresOrdenados [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]
-}


{-
Ejercicio 7.20.2. Definir, por plegado, la función
    paresOrdenados2 :: [a] -> [(a,a)]
tal que (paresOrdenados2 xs) es la lista de todos los pares de elementos (x,y) de xs, tales
que x ocurren en xs antes que y. Por ejemplo,
    paresOrdenados2 [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
    paresOrdenados2 [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]
-}


{-
Ejercicio 7.20.3. Definir, usando repeat, la función
    paresOrdenados3 :: [a] -> [(a,a)]
tal que (paresOrdenados3 xs) es la lista de todos los pares de elementos (x,y) de xs, tales
que x ocurren en xs antes que y. Por ejemplo,
    paresOrdenados3 [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
    paresOrdenados3 [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]
-}


{-
Ejercicio 7.21.1. Definir, por recursión, la función
    potenciaFunc :: Int -> (a -> a) -> a -> a
tal que (potenciaFunc n f x) es el resultado de aplicar n veces la función f a x. Por ejemplo,
    potenciaFunc 3 (*10) 5 == 5000
    potenciaFunc 4 (+10) 5 == 45
-}

{-
Ejercicio 7.21.2. Definir, sin recursión, la función
    potenciaFunc2 :: Int -> (a -> a) -> a -> a
tal que (potenciaFunc2 n f x) es el resultado de aplicar n veces la función f a x. Por ejemplo,
    potenciaFunc2 3 (*10) 5 == 5000
    potenciaFunc2 4 (+10) 5 == 45
-}


{-
Ejercicio 7.22.1. Definir, por recursión, la función
    sumaDeDos :: Int -> [Int] -> Maybe (Int,Int)
tal que (sumaDeDos x ys) decide si x puede expresarse como suma de dos elementos de ys y,
en su caso, devuelve un par de elementos de ys cuya suma es x. Por ejemplo,
    sumaDeDos 9 [7,4,6,2,5] == Just (7,2)
    sumaDeDos 5 [7,4,6,2,5] == Nothing
-}


{-
Ejercicio 7.22.2. Definir, usando la función paresOrdenados (definida en la página 164), la
función
    sumaDeDos' :: Int -> [Int] -> Maybe (Int,Int)
tal que (sumaDeDos' x ys) decide si x puede expresarse como suma de dos elementos de ys y,
en su caso, devuelve un par de elementos de ys cuya suma es x. Por ejemplo,
    sumaDeDos' 9 [7,4,6,2,5] == Just (7,2)
    sumaDeDos' 5 [7,4,6,2,5] == Nothing
-}


{-
Ejercicio 7.23.1. Definir la función
    eslabones :: Int -> Int -> Int -> [Int]
tal que (eslabones i d n) es la lista con los números de eslabones que tocan el radio doblado
en cada vuelta en una bicicleta de tipo (i,d,n). Por ejemplo,
    take 10 (eslabones 2 7 25) == [2,9,16,23,5,12,19,1,8,15]
-}


{-
Ejercicio 7.23.2. Definir la función
    numeroVueltas :: Int -> Int -> Int -> Int
tal que (numeroVueltas i d n) es el número de vueltas que pasarán hasta que la cadena se
rompa en una bicicleta de tipo (i,d,n). Por ejemplo,
    numeroVueltas 2 7 25 == 14
-}


{-
Ejercicio 7.24.1. Definir la función
    golomb :: Int -> Int
tal que (golomb n) es el n–ésimo término de la sucesión de Golomb. Por ejemplo,
    golomb 5 == 3
    golomb 9 == 5

Indicación: Se puede usar la función sucGolomb del siguiente ejercicio.
-}


{-
Ejercicio 7.24.2. Definir la función
    sucGolomb :: [Int]
tal que sucGolomb es la lista de los términos de la sucesión de Golomb. Por ejemplo,
    take 15 sucGolomb == [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]

Indicación: Se puede usar la función subSucGolomb del siguiente ejercicio.
-}


{-
Ejercicio 7.24.3. Definir la función
    subSucGolomb :: Int -> [Int]
tal que (subSucGolomb x) es la lista de los términos de la sucesión de Golomb a partir de la
primera ocurrencia de x. Por ejemplo,
    take 10 (subSucGolomb 4) == [4,4,4,5,5,5,6,6,6,6]

Indicación: Se puede usar la función golomb del ejercicio anterior.
-}
