-- Trabajo Final Programación Declarativa.

-- Desarrollado por los alumnos:
-- Manuel Correa Gomez
-- Alejandro Jiménez Martín

-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

import Mapa

-- Función que agrega una línea en blanco
lineaBlanco = do
    putChar '\n'


frozenLake = do
    lineaBlanco
    putStrLn "Introduce el tamaño del tablero:"
    x <- getLine
    putStrLn "Introduce un índice:"
    y <- getLine
    let n = read x :: Int
    let idx = read y :: Int 
    lineaBlanco
    crearTablero n idx



