
{-
Practica 07
Definiciones de árboles.

EQUIPO:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

    Objetivo: Practica 7, Definiciones de árboles.
    Representación de Ia información en estructuras de árbol y su manipulación para Ia solución de
    problemas. Se corresponde con el Tema 2 “Programación Funcional” del programa de la
    materia
-}
{-
En los siguientes ejercicios se trabajará con el tipo de datos algebraico de los árboles binarios
definidos como sigue:
-}
data Arbol = Hoja Int | Nodo Int Arbol Arbol deriving Show

ejArbol :: Arbol
ejArbol = Nodo 3 (Nodo 4 (Hoja 1) (Hoja 2)) (Hoja 5)

{-
7.1 Definir la función ocurre :: Int -> Arbol -> Bool, tal que (ocurre x a) se verifica si x
ocurre en el árbol a como valor de un nodo o de una hoja.
Por ejemplo,
ocurre 4 ejArbol == True
ocurre 10 ejArbol == False
-}
ocurre :: Int -> Arbol -> Bool
ocurre x (Hoja valor) = x == valor
ocurre x (Nodo valor izq der) = x == valor || ocurre x izq || ocurre x der

{-
7.2 En el preludio está definido el tipo de datos
 data Ordering = LT | EQ | GT
junto con la función
 compare :: Ord a => a -> a -> Ordering
que decide si un valor en un tipo ordenado es menor (LT), igual (EQ) o mayor (GT) que otro.
Usando esta función, redefinir la función
ocurre2 :: Int -> Arbol -> Bool
del ejercicio anterior.
-}
ocurre2 :: Int -> Arbol -> Bool
ocurre2 x (Hoja valor) = compare x valor == EQ
ocurre2 x (Nodo valor izq der) = compare x valor == EQ || ocurre2 x izq || ocurre2 x der


{-
7.3 En los siguientes ejercicios se trabajará con el tipo algebraico de dato de los árboles
binarios con valores en las hojas definido por
data ArbolB = HojaB Int
 | NodoB ArbolB ArbolB
 deriving Show
Por ejemplo, el árbol
se representa por
ejArbolB :: ArbolB
ejArbolB = NodoB (NodoB (HojaB 1) (HojaB 4))
 (NodoB (HojaB 6) (HojaB 9))

Definir la función
nHojas :: ArbolB -> Int
tal que (nHojas a) es el número de hojas del árbol a. Por ejemplo,
nHojas (NodoB (HojaB 5) (NodoB (HojaB 3) (HojaB 7))) == 3
nHojas ejArbolB == 4
-}
data ArbolB = HojaB Int | NodoB ArbolB ArbolB deriving Show

nHojas :: ArbolB -> Int
nHojas (HojaB _) = 1
nHojas (NodoB izq der) = nHojas izq + nHojas der


{-
7.4 Se dice que un árbol de este tipo es balanceado si es una hoja o bien si para cada
nodo se tiene que el número de hojas en cada uno de sus subárboles difiere como
máximo en uno y sus subárboles son balanceados. Definir la función
balanceado :: ArbolB -> BoolB
tal que (balanceado a) se verifica si a es un árbol balanceado. 
Por ejemplo,
balanceado ejArbolB ==> True
balanceado (NodoB (HojaB 5) (NodoB (HojaB 3) (HojaB 7))) ==> True
balanceado (NodoB (HojaB 5) (NodoB (HojaB 3) (NodoB (HojaB 5) (HojaB 7)))) ==> False
-}
balanceado :: ArbolB -> Bool
balanceado (HojaB _) = True  -- Una hoja es un árbol balanceado
balanceado (NodoB izq der) =
    abs (nHojas izq - nHojas der) <= 1 && balanceado izq && balanceado der

{-
7.5 Definir la función mitades :: [a] -> ([a],[a]) tal que (mitades xs) es un par de listas que
se obtiene al dividir xs en dos mitades cuya longitud difiere como máximo en uno. Por
ejemplo,
mitades [2,3,5,1,4,7] == ([2,3,5],[1,4,7])
mitades [2,3,5,1,4,7,9] == ([2,3,5],[1,4,7,9])
-}
mitades :: [a] -> ([a], [a])
mitades xs = splitAt (length xs `div` 2) xs

{-
7.6 Definir la función
arbolBalanceado :: [Int] -> ArbolB
tal que (arbolBalanceado xs) es el árbol balanceado correspondiente a la lista xs. Por
ejemplo,
arbolBalanceado [2,5,3] ==NodoB (HojaB 2) (NodoB (HojaB 5) (HojaB 3))
arbolBalanceado [2,5,3,7] == NodoB (NodoB (HojaB 2) (HojaB 5)) (NodoB (HojaB 3) (HojaB 7))
-}
arbolBalanceado :: [Int] -> ArbolB
arbolBalanceado [] = error "La lista debe contener al menos un elemento"
arbolBalanceado [x] = HojaB x
arbolBalanceado xs =
    let (izq, (raiz:der)) = splitAt (length xs `div` 2) xs
    in NodoB (arbolBalanceado izq) (arbolBalanceado der)

{-
7.7 En los siguientes ejercicios se trabajará con el tipo algebraico de datos de los
árboles binarios definidos como sigue
data Arbol a = Hoja
 | Nodo a (Arbol a) (Arbol a)
 deriving (Show, Eq)
En los ejemplos se usará el siguiente árbol
arbol = Nodo 9
 (Nodo 3
 (Nodo 2 Hoja Hoja)
 (Nodo 4 Hoja Hoja))
 (Nodo 7 Hoja Hoja)
-}
data ArbolA a = HojaA
             | NodoA a (ArbolA a) (ArbolA a)
             deriving (Show, Eq)

arbol :: ArbolA Int
arbol =
    NodoA 9
        (NodoA 3
            (NodoA 2 HojaA HojaA)
            (NodoA 4 HojaA HojaA))
        (NodoA 7 HojaA HojaA)

{-
7.8 Definir la función
nHojas :: Arbol a -> Int
tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
ghci> arbol
Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
ghci> nHojas arbol
6
5
-}
nHojasA :: ArbolA a -> Int
nHojasA HojaA = 1
nHojasA (NodoA _ izq der) = nHojasA izq + nHojasA der


{-
7.9 Definir la función
nNodos :: Arbol a -> Int
tal que (nNodos x) es el número de nodos del árbol x. Por ejemplo,
ghci> arbol
Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
ghci> nNodos arbol
5
-}
nNodosA :: ArbolA a -> Int
nNodosA HojaA = 0
nNodosA (NodoA _ izq der) = 1 + nNodosA izq + nNodosA der

{-
7.10 Definir la función
profundidad :: Arbol a -> Int
tal que (profundidad x) es la profundidad del árbol x. Por ejemplo,
ghci> arbol
Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
ghci> profundidad arbol
3
-}
profundidadA :: ArbolA a -> Int
profundidadA HojaA = 1 
profundidadA (NodoA _ izq der) = 1 + max (profundidadA izq) (profundidadA der)

{-
7.11 Definir la función
preorden :: Arbol a -> [a]
tal que (preorden x) es la lista correspondiente al recorrido preorden del árbol x; es decir,
primero visita la raíz del árbol, a continuación recorre el subárbol izquierdo y, finalmente,
recorre el subárbol derecho. Por ejemplo,
ghci> arbol
Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
ghci> preorden arbol
[9,3,2,4,7]
-}
preordenA :: ArbolA a -> [a]
preordenA HojaA = [] 
preordenA (NodoA valor izq der) = [valor] ++ preordenA izq ++ preordenA der

{-
7.12 Definir la función
postorden :: Arbol a -> [a]
tal que (postorden x) es la lista correspondiente al recorrido postorden del árbol x; es decir,
primero recorre el subárbol izquierdo, a continuación el subárbol derecho y, finalmente, la raíz
del árbol. Por ejemplo,
ghci> arbol
Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
ghci> postorden arbol
[2,4,3,7,9]
-}
postordenA :: ArbolA a -> [a]
postordenA HojaA = []  
postordenA (NodoA valor izq der) = postordenA izq ++ postordenA der ++ [valor]
