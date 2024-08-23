--Elabore un programa que solicite
--el ingreso por hora de horas trabajadas
--y luego muestre los ingresos totales
getDouble :: IO Double
getDouble = do
    n <- getLine
    return (read n)

main = do
    putStrLn "¿Cual es el pago por hora?"
    pago <-getDouble
    putStrLn "¿Cuantas horas se trabajo?"
    horas <- getDouble
    let total = pago * horas
    putStrLn ("Pago Total: " ++ (show total))