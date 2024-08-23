import Data.Char (chr, ord)
import Data.Char
import Data.List
{-
CIFRADO CESAR

EQUIPO 5:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

Objetivo: Cifrado Cesar
    Definir las funciones necesarias para el cifrado y descifrado de las diapositivas de clase.
-}

--Letra minuscula (a-z) a numero
let2int :: Char -> Int
let2int c = ord c - ord 'a'

--Numero (0 a 25) a letra minuscula
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

--Desplaza el caracter
desplaza :: Int -> Char -> Char
desplaza n c | elem c ['a'..'z'] = int2let ((let2int c+n) `mod` 26) | otherwise = c

--Codifica el texto con desplazamiento
codifica :: Int -> String -> String
codifica n xs = [desplaza n x | x <- xs]

--Propiedad de desplazamiento
prop_desplaza n xs = desplaza (-n) (desplaza n xs) == xs

--Propiedad de codificacion
prop_codifica n xs = codifica (-n) (codifica n xs) == xs

--Tabla de frecuencias ['a'..'z']
tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 0.70, 6.25, 0.44, 0.01, 4.97, 3.15, 6.71, 8.68, 2.51, 0.88, 6.87, 7.98, 4.63, 3.93,  0.90, 0.02, 0.22, 0.90, 0.52]

--Porcentaje
porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100

--ninusculas
minusculas :: String -> Int
minusculas xs = length [ x | x <- xs, isLower x]

--ocurrencias
ocurrencias :: Char -> String -> Int
ocurrencias x xs = length [ x' | x' <- xs, x==x']

--Frecuencias
frecuencias :: String -> [Float]
frecuencias xs = [porcentaje (ocurrencias x xs) n | x <- ['a'..'z']] where n = minusculas xs

--Chi Cuadrado
chiCuad :: [Float] -> [Float] -> Float
chiCuad os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

--Rotar N posiciones
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

--Posiciones
posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs = [ i' | (x', i') <- zip xs [0..n], x==x' ] where n = length xs - 1

--DESCIFRADO
descifra :: String -> String
descifra xs = codifica (-factor) xs where 
    factor = head (posiciones (minimum tabChi) tabChi)
    tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
    tabla' = frecuencias xs
