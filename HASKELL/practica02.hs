{-
PRACTICA 2

EQUIPO:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

Objetivo: Practica 2, Ejercicios.
    Reafirmar y poner en práctica el conocimiento sobre Ia definición de funciones 
    y su uso , así como los tipos de datos.
-}

{-
2.1. Volumen de la esfera
Definir la función volumenEsfera tal que (volumenEsfera r) es el volumen de la esfera de radio r.
Por ejemplo,
volumenEsfera 10 == 4188.790204786391
-}
volumenEsfera r = (4/3)*(pi)*(r^3)

{-
2.2. Área de una corona circular
Definir la función areaDeCoronaCircular tal que (areaDeCoronaCircular r1 r2) es el área de una
corona circular de radio interior r1 y radio exterior r2. Por ejemplo,
areaDeCoronaCircular 1 2 == 9.42477796076938
areaDeCoronaCircular 2 5 == 65.97344572538566
areaDeCoronaCircular 3 5 == 50.26548245743669
-}
areaDeCoronaCircular r1 r2 = pi*((r2^2)-(r1^2))

{-
2.3. Última cifra de un número
Definir la función ultimaCifra tal que (ultimaCifra x) es la última cifra del número x. Por ejemplo,
ultimaCifra 325 == 5
-}
ultimaCifra x = rem x 10

{-
2.4. Máximo de 3 elementos
Definir la función maxTres tal que (maxTres x y z) es el máximo de x, y y z. Por ejemplo,
maxTres 6 2 4 == 6
maxTres 6 7 4 == 7
maxTres 6 7 9 == 9
-}
maxTres x y z = max  (max x y) z

{-
2.5. Mediano de 3 números
Definir la función mediano tal que (mediano x y z) es el número mediano de los tres números x,
y y z. Por ejemplo,
mediano 3 2 5 == 3
mediano 2 4 5 == 4
mediano 2 6 5 == 5
mediano 2 6 6 == 6
-}
mediano x y z = x + y + z - minimum [x,y,z] - maximum [x,y,z]



{-2.6. Igualdad y diferencia de 3 elementos-}
{-2.6.1 Definir la función tresIguales tal que (tresIguales x y z) se verifica si los elementos x, y y z
son iguales. Por ejemplo,
tresIguales 4 4 4 == True
tresIguales 4 3 4 == False
-}
tresIguales x y z = (x == y) && (x == z)

{-
2.6.2. Definir la función tresDiferentes tal que (tresDiferentes x y z) se verifica si los elementos
x, y y z son distintos. Por ejemplo,
tresDiferentes 3 5 2 == True
tresDiferentes 3 5 3 == False
-}
tresDiferentes x y z = (x /= y) && (y /= z) && (y /= z)

{-
2.7. Igualdad de 4 elementos
Definir la función cuatroIguales tal que (cuatroIguales x y z u) se verifica si los elementos x, y, z
y u son iguales. Por ejemplo,
cuatroIguales 5 5 5 5 == True
cuatroIguales 5 5 4 5 == False

Indicación: Usar la función tresIguales.
-}
cuatroIguales x y z u = (u == x) && (tresIguales x y z)

{-
2.8. Propiedad triangular
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
2.9. División segura
Definir la función divisionSegura tal que (divisionSegura x y) es x y si y no es cero y 9999 en
caso contrario. Por ejemplo,
divisionSegura 7 2 == 3.5
divisionSegura 7 0 == 9999.0
-}
divisionSegura (_,0) = 9999
divisionSegura (x,y)= x / y

{-
2.10. Módulo de un vector
Definir la función modulo tal que (modulo v) es el módulo del vector v. Por ejemplo,
modulo (3,4) == 5.0
-}
modulo (x,y) = sqrt((x**2)+(y**2))