import Data.Char
nombre = do
    putStr "Cual es tu nombre? "
    name <- getLine
    let bigName = map toUpper name
    let lon = length name
    putStrLn $ "Nombre: " ++ bigName
    print "Numero de letras:"
    print lon