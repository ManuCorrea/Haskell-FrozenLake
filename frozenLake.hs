import Data.Array as A
import Data.Matrix as M
import Data.Vector as V

import System.Random

--import Graphics.UI.Gtk.Gdk.Events

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

type Estado = (Double, Double)

type Posicion = (Int, Int)

type Tablero = Matrix Char
getRandomNum = randomRIO (0, 1) :: IO Float -- usar asignando num <- getRandomNum


iniciaTablero :: Int -> Tablero
iniciaTablero n = matrix n n $ \(i, j) -> if (i==1 && j==1) then 'S'
                                          else if (i == n && j == n) then 'M'
                                          else 'H'



-- Calculamos el porcentaje estableciendo siempre el 20%
porcentaje n = fromIntegral $ round ((x*20)/100) 
    where x = n*n 

-- Método que calcula si es frontera, por lo que toda posición que sea frontera o "A", no se podrá pasar
esFrontera :: Posicion -> Bool
esFrontera p = undefined
    



-- inicio :: Estado
-- inicio :: Posicion
-- inicio = (0, 0)

-- movimiento :: Event -> Estado -> Estado
-- movimiento (KeyPress "Right") (x, y) = (x+1, y)
-- movimiento (KeyPress "Left") (x, y) = (x-1, y)
-- movimiento (KeyPress "Up") (x, y) = (x, y+1)
-- movimiento (KeyPress "Down") (x, y) = (x, y-1)
-- movimiento _ (x, y) = (x, y)