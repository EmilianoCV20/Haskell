import Test.QuickCheck
import Control.Monad (unless)
import Data.Char(toLower)
{-
Practica 04
Definicion de Funciones

EQUIPO:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

    Objetivo: Practica 4, Definicion de Funciones
    Reafirmar y poner en practica el conocimiento sobre  la definicion de 
    funciones y sus usos, asi como los tipos de datos. En esta practica se plantean
    ejercicios con definiciones elementales (no recursivas) de funciones.
    Se corresponde con el Tema 1 "Conceptos fundamentales" del programa de la materia
-}
{-
4.1. Media de 3 números
Definir la función media3 tal que (media3 x y z) es la media aritmética de los números x, y y z.
Por ejemplo,
media3 1 3 8 == 4.0
media3 (-1) 0 7 == 2.0
media3 (-3) 0 3 == 0.0
-}
media3 x y z = (x+y+z) / 3

{-
4.2. Números iguales.
Que pida 3 números y los muestre en pantalla de mayor a menor en líneas distintas. En caso
de haber números iguales se muestran en la misma línea.
-}
numerosIguales :: [Double] -> String
numerosIguales [x, y, z]
    | x == y && y == z = show x ++ " " ++ show y ++ " " ++ show z
    | x == y = show x ++ " " ++ show y ++ "\n" ++ show z
    | y == z = show x ++ "\n" ++ show y ++ " " ++ show z
    | otherwise = show x ++ "\n" ++ show y ++ "\n" ++ show z

{-
4.3. Volumen de la esfera
Definir la función volumenEsfera tal que (volumenEsfera r) es el volumen de la esfera de radio r.
Por ejemplo,
volumenEsfera 10 == 4188.790204786391
Indicación: Usar la constante pi.
-}
volumenEsfera r = (4/3)*(pi)*(r^3)

{-
4.4. Área de una corona circular
Definir la función areaDeCoronaCircular tal que (areaDeCoronaCircular r1 r2) es el área de una
corona circular de radio interior r1 y radio exterior r2. Por ejemplo,
areaDeCoronaCircular 1 2 == 9.42477796076938
areaDeCoronaCircular 2 5 == 65.97344572538566
areaDeCoronaCircular 3 5 == 50.26548245743669
-}
areaDeCoronaCircular r1 r2 = pi*((r2^2)-(r1^2))

{-
4.5. Valida la entrada
Elabore una función que sólo permita introducir los caracteres S y N.
-}
pedirSN :: IO Char
pedirSN = do
    putStrLn "Por favor, ingrese 'S' o 'N':"
    input <- getLine

    -- Verificar si la entrada es válida (solo 'S' o 'N')
    if input `elem` ["S", "s", "N", "n"]
        then return (head input)
        else do
            putStrLn "Entrada no válida. Debe ingresar 'S' o 'N'."
            -- Llamar recursivamente a la función hasta que se ingrese una entrada válida.
            pedirSN

{-
4.6. Mediano de 3 números
Definir la función mediano tal que (mediano x y z) es el número mediano de los tres números x,
y y z. Por ejemplo,
mediano 3 2 5 == 3
mediano 2 4 5 == 4
mediano 2 6 5 == 5
mediano 2 6 6 == 6
-}
mediano x y z = x + y + z - minimum [x,y,z] - maximum [x,y,z]

{-
4.7. Propiedad triangular
Las longitudes de los lados de un triángulo no pueden ser cualesquiera. Para que pueda
construirse el triángulo, tiene que cumplirse la propiedad triangular; es decir, longitud de cada
lado tiene que ser menor que la suma de los otros dos lados. Definir la función triangular tal que
(triangular a b c) se verifica si a, b y c cumplen la propiedad triangular. Por ejemplo,
triangular 3 4 5 == True
triangular 30 4 5 == False
triangular 3 40 5 == False
triangular 3 4 50 == False
-}
triangular a b c = (a <= b+c) && (b <= a+c) && (c <= a+b)

{-
4.8. Mayor de edad
Escribir un programa que pregunte al usuario su edad y muestre por pantalla si es mayor de
edad o no.
-}
esMayorDeEdad :: Int -> String
esMayorDeEdad edad
        | edad >= 18 = "Eres MAYOR de Edad"
        | otherwise = "Eres menor de Edad" 

{-
4.9 Contraseña
Escribe una funcion que almacene la cadena de caracteres “Contraseña” en una variable,
pregunte al usuario por la contraseña e imprima por pantalla si la contraseña introducida por el
usuario coincide con la guardada en la variable sin tener en cuenta mayúsculas y minúsculas.
-}
verificarContraseña :: String -> IO ()
verificarContraseña contraseñaGuardada = do
    putStrLn "Por favor, ingrese su contraseña:"
    contraseñaUsuario <- getLine

    let contraseñaGuardadaMin = map toLower contraseñaGuardada
        contraseñaUsuarioMin = map toLower contraseñaUsuario

    if contraseñaUsuarioMin == contraseñaGuardadaMin
        then putStrLn "¡La contraseña es correcta!"
        else putStrLn "La contraseña es incorrecta."

{-
4.10 Primo
Escriba la función esPrimo :: Int -> Bool tal que, dado un número natural diga si es un número
primo o no.
-}
factoresDe :: Int -> [Int]
factoresDe n = [x | x <- [1..n], n `mod` x == 0]

esPrimo :: Int -> Bool
esPrimo n = factoresDe n == [1, n]