--Posicion
type Pos = (Int,Int)

origen :: Pos
origen = (0,0)

izquierda :: Pos -> Pos
izquierda (x,y) = (x-1,y)

--Parametrizada
type Par a = (a,a)

multiplica :: Par Int -> Int
multiplica (x,y) = x*y

copia :: a -> Par a
copia x = (x,x)

--Uso de los valores de los tipos definidos
data Mov = Izquierda | Derecha | Arriba | Abajo

movimiento :: Mov -> Pos -> Pos
movimiento Izquierda (x,y) = (x-1,y)
movimiento Derecha (x,y) = (x+1,y)
movimiento Arriba (x,y) = (x,y+1)
movimiento Abajo (x,y) = (x,y-1)

movimientos :: [Mov] -> Pos -> Pos
movimientos []            p = p
movimientos (m:ms) p = movimientos ms (movimiento m p)

opuesto :: Mov -> Mov
opuesto Izquierda = Derecha
opuesto Derecha = Izquierda
opuesto Arriba = Abajo
opuestoAbajo=Arriba

--Definición de tipo con constructores con parámetros
data Figura = Circulo Float | Rect Float Float

cuadrado :: Float -> Figura
cuadrado n = Rect n n

area :: Figura -> Float
area (Circulo r) = pi*r^2
area (Rect x y) = x*y


--Definición de tipos recursivos: Los naturales
data Nat = Cero | Suc Nat
                         deriving Show

nat2int :: Nat -> Int
nat2int Cero = 0
nat2int (Suc n) = 1 + nat2int n

suma :: Nat -> Nat -> Nat
suma Cero       n = n
suma (Suc m) n = Suc (suma m n)

int2nat :: Int -> Nat
int2nat 0 = Cero
int2nat (n+1) = Suc (int2nat n)
-- :set -XHaskell98

--Tipo recursivo con parámetro: Las listas
data Lista a = Nil | Cons a (Lista a)

longitud :: Lista a -> Int
longitud Nil                       = 0
longitud (Cons _ xs) = 1 + longitud xs

--Definición de tipos recursivos: Los árboles binarios
data Arbol = Hoja Int | Nodo Arbol Int Arbol

ejArbol = Nodo (Nodo (Hoja 1) 3 (Hoja 4))
                                     5
                                      (Nodo (Hoja 6) 7 (Hoja 9))

ocurre :: Int -> Arbol -> Bool
ocurre m (Hoja n) = m == n
ocurre m (Nodo i n d) = m == n || ocurre m i || ocurre m d

aplana :: Arbol -> [Int]
aplana (Hoja n) = [n]
aplana (Nodo i n d) = aplana i ++ [n] ++ aplana d

ocurreEnArbolOrdenado :: Int -> Arbol -> Bool
ocurreEnArbolOrdenado m (Hoja n) = m == n
ocurreEnArbolOrdenado m (Nodo i n d)
                                                            | m == n = True
                                                            | m < n = ocurreEnArbolOrdenado m i
                                                            | otherwise = ocurreEnArbolOrdenado m d

--Definiciones de distintos tipos de árboles:

    --Árboles binarios con valores en las hojas:
        --data Arbol a = Hoja a | Nodo (Arbol a) (Arbol a)

    --Árboles binarios con valores en los nodos:
        --data Arbol a = Hoja | Nodo (Arbol a) a (Arbol a)

    --Árboles binarios con valores en las hojas y en los nodos:
        --data Arbol a b = Hoja a | Nodo (Arbol a b) b (Arbol a b)

    --Árboles con un número variable de sucesores:
        --data Arbol a = Nodo a [Arbol a]
