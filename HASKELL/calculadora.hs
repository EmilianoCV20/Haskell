import Graphics.UI.Gtk
{-
Practica Final de Haskell
Hacer una aplicación GUI en Haskell

EQUIPO:
    Nombre: Marcela Sofia Wong Carranza
    Numero de control: 18130612

    Nombre: Iván Jhair Gómez Rincón 
    Numero de control: 20130019

    Nombre: Emiliano Cepeda Villarreal
    Numero de control: 20130792

    Objetivo: Practica Final de Haskell
    Que el alumno conozca y se familiarice con las herramientas que utilizara para Ia programación
    GUI en lenguaje HASKELL. Se corresponde con el Tema 1 y 2 del programa de la materia.
-}

main :: IO ()
main = do
    -- Inicializa GTK
    void initGUI

    -- Crea la ventana principal
    window <- windowNew
    set window [windowTitle := "Calculadora Básica", containerBorderWidth := 10]

    -- Caja de entrada para mostrar los números y resultados
    entry <- entryNew
    set entry [entryEditable := False]

    -- Crea una tabla para organizar los botones de la calculadora
    table <- tableNew 4 4 True
    containerAdd window table

    -- Definir botones
    let buttons = ["7", "8", "9", "/",
                   "4", "5", "6", "*",
                   "1", "2", "3", "-",
                   "0", "C", "=", "+"]

    -- Crea y agrega los botones a la tabla
    entries <- mapM (\label -> buttonNewWithLabel label) buttons
    mapM_ (\(button, row, col) -> tableAttachDefaults table button col (col + 1) row (row + 1)) (zip3 entries [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3] [0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3])

    -- Conecta los botones a la función de manejo de eventos
    mapM_ (\(button, label) -> onClicked button (onButtonClicked entry label)) (zip entries buttons)

    -- Manejo de eventos para los botones
    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

-- Función para manejar los clics de botón
onButtonClicked :: Entry -> String -> IO ()
onButtonClicked entry label
    | label == "=" = do
        result <- entryGetText entry
        let parsedResult = read (result :: String) :: Double
        entrySetText entry (show parsedResult)
    | label == "C" = entrySetText entry ""
    | otherwise = do
        currentText <- entryGetText entry
        entrySetText entry (currentText ++ label)
