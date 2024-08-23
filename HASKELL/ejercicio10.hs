getDouble :: IO Double
getDouble = do
    s <- getLine
    return (read s)

main = do
    putStrLn "Cuantas Muñecas compradas?"
    muneca <- getDouble
    putStrLn "Cuantos Payasos comprados?"
    payaso <-getDouble
    let peso = (payaso * 112) + (muneca * 75)
    putStrLn ("Se vendieron "++(show payaso)++ " payaso(s), " ++(show muneca)++" muñeca(s)" ++ " y el Paquete pesa: " ++ (show peso) ++ "g")