import Data.Array as A
import Data.Matrix as M
import Data.Vector as V

import System.Random

{-
Vamos a modelar la cuadrícula con el tipo Matrix de la librería Data
En la cuadrícula vamos a tener:
    *H: Helado
    *S: Salida
    *M: Meta
    *A: Agujero

    Tendremos una variable Estado con el que tendremos las
    coordenadas de la posición del agente.

    Recompensa +1 si alcanza la meta, en otro caso 0
    Se termina cuando el agente alcanza M.
-}

type Tablero = Matrix Char
getRandomNum = randomRIO (0, 1) :: IO Float -- usar asignando num <- getRandomNum


iniciaTablero :: Int -> Tablero
iniciaTablero n = matrix n n (\(i, j) -> if (i==1 && j==1) then 'S'
                                         else if (i == n && j == n) then 'M'
                                         else 'H')
