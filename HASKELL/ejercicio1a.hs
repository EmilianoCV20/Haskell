--Escriba un Programa que Escriba la Cadena "Hola Mundo!"--
saludo :: IO()
saludo =  putStrLn ("Hola, Mundo!")

saludo2 :: IO()
quien = "Mundo"
saludo2 = putStrLn ( "Hola " ++ quien ++ "!" )

saludo3 :: [Char] -> IO()
saludo3 x = putStrLn $"Hola, " ++ x ++ "!"
--saludo3 "MUNDO"