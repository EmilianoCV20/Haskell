--Escibe un programa que pida el nombre, edad, trabajo y locacion--
main = do
    putStrLn "Hola, dime tu nombre:"
    nombre <- getLine
    putStrLn "Dime tu Edad:"
    edad <- getLine
    putStrLn "Dime tu Trabajo"
    trabajo <- getLine
    putStrLn "Donde estas?"
    lugar <- getLine
    putStrLn ("Hola " ++ nombre ++ " de " ++ edad ++ " que trabaja de " ++ trabajo ++ " y esta en " ++ lugar)
    