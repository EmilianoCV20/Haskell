import Data.Char (isDigit)
import Data.List (sort)
import Data.Function (on)
import Control.Monad.Cont
import Control.Monad (forM_)
{-
Practica 05
Definiciones de Listas

EQUIPO:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

    Objetivo: Practica 5, Definiciones de Listas
    Reafirmar y poner en practica el conocimiento sobre  la definicion de 
    listas y sus usos, asi como los tipos de datos.
-}
{-
5.1 Escriba una función en haskell:
a) Que asigne a la variable nps una lista definida por enumeración de números enteros
pares comprendidos entre 1 y 10.
b) Que asigne a la variable vns una lista de números del 1 al 10 construida a partir de la
lista vacía y el uso del operador “:”
c) Que asigne a la variable ins una lista definida con intervalos de los números pares
comprendidos entre 1 y 100
d) Que asigne a la variable cns una lista definida por comprensión que contenga los
cuadrados de los números nones contenidos en la lista vns (utilice una guarda).
e) Que construya por comprensión y utilizando mas de un generador la lista [(1,4),(1,5),
(2,4),(2,5),(3,4),(3,5)], asignela a la variable gts.
f) Imprima las listas obtenidas
-}
listas = do
    --A
    let nps = [2, 4, 6, 8, 10]
    print(show nps)
    --B
    let vns = 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : []
    print(show vns)
    --C
    let ins = [2, 4..100]
    print(show ins)
    --D
    let cns = [x*x | x <- vns, odd x]
    print(show cns)
    --E
    let gts = [(x, y) | x <- [1..3], y <- [4, 5]]
    print(show gts)


{-
5.2 Elabore una función que dada una lista imprima el primer y segundo elemento.
-}
primeroYSegundo :: Show a => [a] -> IO ()
primeroYSegundo [] = putStrLn "La lista está vacía."
primeroYSegundo [x] = putStrLn "La lista tiene solo un elemento."
primeroYSegundo (x:y:_) = do
    putStrLn $ "El primer elemento es: " ++ show x
    putStrLn $ "El segundo elemento es: " ++ show y

{-
5.3 Elabore una función que pida 5 números enteros, los guarde en la lista “numeros” y
muestre la suma de los números pares y de los nones.
-}
obtenerNumeros :: Int -> ([Int], [Int]) -> IO ([Int], [Int])
obtenerNumeros 0 numeros = return numeros
obtenerNumeros n (pares, nones) = do
    putStrLn $ "Introduce un número entero (faltan " ++ show n ++ "):"
    input <- getLine
    if all isDigit input
        then do
            let numero = read input
            if even numero
                then obtenerNumeros (n - 1) (numero : pares, nones)
                else obtenerNumeros (n - 1) (pares, numero : nones)
        else do
            putStrLn "Entrada no válida. Introduce un número entero válido:"
            obtenerNumeros n (pares, nones)

{-
5.4 Elabore una función que dada una lista muestre el valor mínimo, el máximo, la media,
la moda, la varianza y la desviación estandar (consulte si haskell tiene implementadas
las funciones para cada caso).
-}

-- Función para calcular el valor mínimo de una lista
minimo :: (Ord a) => [a] -> a
minimo = minimum
-- Función para calcular el valor máximo de una lista
maximo :: (Ord a) => [a] -> a
maximo = maximum
-- Función para calcular la media de una lista de números
media :: (Fractional a) => [a] -> a
media xs = sum xs / fromIntegral (length xs)
-- Función para calcular la varianza de una lista de números
varianza :: (Fractional a) => [a] -> a
varianza xs = sum [(x - m) ^ 2 | x <- xs] / fromIntegral (length xs)
    where m = media xs

{-
5.5 Considere la lista: names = ["Alicia", "Bob", "Cindy", "Daisy", "Erik", "Fernando"], 
elabore una función para iterar sobre la lista imprimiendo cada nombre, utilice mapM_
-}
nombre = do
    let names = ["Alicia", "Bob", "Cindy", "Daisy", "Erik", "Fernando"]
    print names

    mapM_ putStrLn names

{-
5.6 Haga una función que muestre una tabla de multiplicar. (utilice forM, consulte forM y
forM_).
-}
tabla n = do
    forM_ [1..10] $ \i -> do
        let total = n * i
        print(show n ++ " * " ++ show i ++ " = " ++ show total)

{-
5.7 Escribe una función myLast :: [a] -> a que, dada una lista de elementos de tipo a,
retorna el último elemento de la lista. (implementela con recursión).
-}
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

{-
5.8 Escribe una función myLast2 :: [a] -> a que, dada una lista de elementos de tipo a,
retorna el último elemento de la lista. (implementela con composición de funciones).
-}
myLast2 :: [a] -> a
myLast2 = head . reverse

{-
5.9 Escribe una función penultimo :: [a] -> a que, dada una lista de elementos de tipo a,
retorna el penúltimo elemento de la lista. (implementela con recursión).
-}
penultimo :: [a] -> a
penultimo [] = error "La lista está vacía." 
penultimo [_] = error "La lista tiene solo un elemento."
penultimo [x, _] = x
penultimo (_:xs) = penultimo xs 

{-
5.10 Escribe una función penultimo2 :: [a] -> a que, dada una lista de elementos de tipo
a, retorna el penúltimo elemento de la lista. (implementela con composición de
funciones).
-}
penultimo2 :: [a] -> a
penultimo2 = last . init