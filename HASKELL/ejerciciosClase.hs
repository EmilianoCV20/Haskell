{-Ejercicio 1: Defina la funcion media3 tal que media3 x y z es la media aritmetica 
de los numeros x y z, por ejemplo:
media3 1 3 8 == 4.0
media3 (-1) 0 7 == 2.0
-}
media3 x y z = (x+y+z) / 3

{-Ejercicio 1.2: Defina la funcion sumaMonedas tal que sumaMonedas a b c d e es la suma
de los pesos correspondientes: a 1 peso, b 2 pesos, c 5 pesos, d 10 pesos y e 20 pesos
Por ejemplo:
sumaMonedas 0 0 0 0 1 == 20
sumaMonedas 0 0 8 0 3 == 100
-}
sumaMonedas a b c d e = a+(b*2)+(c*5)+(d*10)+(e*20)

{- Ejercicio 1.3: Elabore la funcion volumenEsfera tal que volumenEsfera r 
es el volumen de la esfera de radio r. Por Ejemplo:
volumenEsfera 10 == 4188.79
-}
volumenEsfera r = (4/3)*(pi)*(r^3)

{-Ejercicio 1.4: Defina la funcion areaDeCoronaCircular tal que
areaDeCoronaCircular r1 r2 es el area de una corona circular de
rario interior r1 y radio exterior r2. Por Ejemplo:
areaDeCoronaCircular 1 2 = 9.42
areaDeCoronaCircular 3 5 = 50.26
-}
areaDeCoronaCircular r1 r2 = pi*((r2^2)-(r1^2))

{-Ejercicio 1.5: Defina la funcion ultimoDigito, tal que ultimoDigito x es el ultimo digito
del numero x. Por Ejemplo:
ultimoDigito 1467 == 7
-}
ultimoDigito x = rem x 10