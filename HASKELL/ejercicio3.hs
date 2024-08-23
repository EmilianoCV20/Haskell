--Escribe un programa que pregunte el nombre y salude--
main = do
    putStrLn "Hola, dime tu nombre:"
    nombre <- getLine
    putStrLn ("Hola " ++ nombre ++"!!!!!")
