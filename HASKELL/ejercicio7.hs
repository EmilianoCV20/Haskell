--Escribe un programa que calcule el indice de masa corporal
--Pida el peso y altura
--bmi = peso (kg) / (altura * altura) (m)
getDouble :: IO Double
getDouble = do
    s <- getLine
    return (read s)

main = do
    putStrLn "Peso del Individuo:"
    peso <- getDouble
    putStrLn "Altura del Individuo"
    altura <-getDouble
    let bmi = peso/(altura * altura)
    putStrLn ("El Indice de masa corporal (kg/m^2) es: " ++ (show bmi))
    --print bmi