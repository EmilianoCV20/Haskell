{-
Practica 06
Uso de funciones que operan con listas

EQUIPO:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

    Objetivo: Practica 6, Uso de funciones que operan con listas
    Reafirmar y poner en practica el conocimiento sobre  las funciones de 
    listas y sus usos, asi como los tipos de datos.
-}
{-
6.1 Considere un par de listas que representan las calificaciones de dos grupos de
alumnos:
l1=[80,50,70,60,90,85,75,60,95,77]
l2=[60,40,100,97,82,64,62,44,84,90]
Elabore una función haskell para cada inciso a continuación, en cada función deberá
definir las listas:
a) Concatene las listas para obtener una sola con todas las calificaciones.
b) Calcule el promedio de las calificaciones.
c) Calcule la media, la varianza y la desviación estándar. De ser posible utilice “Map”.
d) Obtenga la lista 3 con los 10 primeros elementos de la lista 1 y los 10 últimos de la lista 2.
e) Obtenga la lista 4 con dos elementos, el máximo y el mínimo de la lista 3
f) Utilice la función takeWhile para obtener los primeros elementos de la lista 3 menores a 70
g) Con la lista 3 utilice “filter” para obtener la lista de los números pares y la lista de los números
nones. 
-}
listas = do
    let l1 = [80,50,70,60,90,85,75,60,95,77,99,55]
    let l2 = [60,40,100,97,82,64,62,44,84,90,99,55]

    --A
    let concatena = l1 ++ l2
    print( show concatena)

    --B
    let promedio  = fromIntegral (sum (l1 ++ l2)) / fromIntegral (length (l1 ++l2))
    print( show promedio)

    --C
    let media = fromIntegral (sum (l1 ++ l2))/fromIntegral (length (l1 ++l2))
    print( show media)
    let varianza = sum (map (\x -> (fromIntegral x - media) ** 2) (l1 ++l2))/fromIntegral (length (l1 ++l2))
    print( show varianza )
    let desviacion = sqrt varianza
    print( show desviacion)
    
    --D
    let obtenerLista3 = take 10 l1 ++ drop (length l2 - 10) l2
    print(show obtenerLista3)

    --E
    let obtenerLista4 = [maximum obtenerLista3, minimum obtenerLista3]
    print(show obtenerLista4)

    --F
    let filtrarMenoresA70 = takeWhile (< 70) obtenerLista3
    print(show filtrarMenoresA70)

    --G
    let filtrarPares  = filter even obtenerLista3
    print(show filtrarPares)
    let filtrarImpares = filter odd obtenerLista3
    print(show filtrarImpares)



{-
6.2 Definir la función rota1 tal que (rota1 xs) es la lista obtenida poniendo el primer
elemento de xs al final de la lista. Por ejemplo,
rota1 [3,2,5,7] == [2,5,7,3]
-}
rota1 :: [a] -> [a]
rota1 [] = []
rota1 (x:xs) = xs ++ [x]

{-
6.3 Definir la función rota tal que (rota n xs) es la lista obtenida poniendo los n primeros
elementos de xs al final de la lista. Por ejemplo,
rota 1 [3,2,5,7] == [2,5,7,3]
rota 2 [3,2,5,7] == [5,7,3,2]
rota 3 [3,2,5,7] == [7,3,2,5]
-}
rota :: Int -> [a] -> [a]
rota n xs
    | n <= 0 = xs
    | n >= length xs = xs 
    | otherwise = drop n xs ++ take n xs

{-
6.4 Rango de una lista
Definir la función rango tal que (rango xs) es la lista formada por el menor y mayor 
elemento de xs. Por ejemplo,
rango [3,2,7,5] == [2,7]

Indicación: Se pueden usar minimum y maximum.
-}
rango :: Ord a => [a] -> [a]
rango xs = [minimum xs, maximum xs]

{-
6.5. Reconocimiento de palíndromos
Definir la función palindromo tal que (palindromo xs) se verifica si xs es un palíndromo; 
es decir, es lo mismo leer xs de izquierda a derecha que de derecha a izquierda. 
Por ejemplo,
palindromo [3,2,5,2,3] == True
palindromo [3,2,5,6,2,3] == False
-}
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

{-
6.6 Elementos interiores de una lista
Definir la función interior tal que (interior xs) es la lista obtenida eliminando los extremos de la
lista xs. Por ejemplo,
interior [2,5,3,7,3] == [5,3,7]
interior [2..7] == [3,4,5,6]
-}
interior :: [a] -> [a]
interior [] = []
interior xs = tail (init xs)

{-
6.7 Segmentos de una lista
Definir la función segmento tal que (segmento m n xs) es la lista de los elementos de xs
comprendidos entre las posiciones m y n. Por ejemplo,
segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
segmento 5 3 [3,4,1,2,7,9,0] == [ ]
-}
segmento :: Int -> Int -> [a] -> [a]
segmento m n xs
    | m > n || m < 1 || n > length xs = []
    | otherwise = take (n - m + 1) (drop (m - 1) xs)

{-
6.8 Extremos de una lista
Definir la función extremos tal que (extremos n xs) es la lista formada por los n primeros
elementos de xs y los n finales elementos de xs. Por ejemplo,
extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
-}
extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs

{-
6.9 Permutación cíclica de una lista
Definir una función ciclo que permute cíclicamente los elementos de una lista, pasando el último
elemento al principio de la lista. Por ejemplo,
ciclo [2, 5, 7, 9] == [9,2,5,7]
ciclo [] == []
ciclo [2] == [2]
-}
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : init xs

{-
(zip xs ys) es la lista obtenida emparejando los elementos de las listas xs e ys.
Por ejemplo,
Prelude> zip ['a','b','c'] [2,5,4,7]
[('a',2),('b',5),('c',4)]

6.10 Defina la función adyacentes xs que es la lista de los pares de elementos 
adyacentes de la lista xs. Por ejemplo,
adyacentes [2,5,3,7] = [(2,5),(5,3),(3,7)]
-}
adyacentes :: [a] -> [(a, a)]
adyacentes xs = zip xs (tail xs)
