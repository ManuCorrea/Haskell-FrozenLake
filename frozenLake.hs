import Data.Array as A hiding ((!))
import Data.Matrix as M
import Data.List

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

type Tablero = Matrix Char
getRandomNum = randomRIO (0, 1) :: IO Float -- usar asignando num <- getRandomNum

obtenerNumAleatorio idx n = take n $ drop (idx*n) (randoms (mkStdGen 11) :: [Float])

iniciaTablero :: Int -> Int -> Tablero
iniciaTablero n idx = M.fromList n n [if (y==1) then 'S' else if (y==n*n)then 'M' else (if (x>0.8) then 'A' else 'H') | (y,x) <- zip [1..(n*n)] (obtenerNumAleatorio idx (n*n))]


-- Calculamos el porcentaje estableciendo siempre el 20%
porcentaje n = fromIntegral $ round ((x*20)/100) 
    where x = n*n 


-- Método que calcula si es frontera, por lo que toda posición que sea frontera o "A", no se podrá pasar
esFrontera m = nub [(i,j) | i <- [1..numF], j <- [1..numC], i==1 || j==1] ++ [(i,j) | i <- [1..numF], j <- [1..numC], i==numF || j==numC]
    where numF = nrows m
          numC = ncols m 

esFrontera2 m = [(i,j) | ((i,j),_) <- indexs m, i==1 || j==1] ++ [(i,j) | ((i,j),_) <- indexs m, i==numF || j==numC]
    where numF = nrows m
          numC = ncols m 

indexs m = [ ((i,j), m!(i,j)) | i <- [1..numF], j <- [1..numC]]
    where numF = nrows m
          numC = ncols m 



-- inicio :: Estado
-- inicio :: Posicion
-- inicio = (1,1)

-- movimiento :: Event -> Estado -> Estado
-- movimiento (KeyPress "Right") (x, y) = (x+1, y)
-- movimiento (KeyPress "Left") (x, y) = (x-1, y)
-- movimiento (KeyPress "Up") (x, y) = (x, y+1)
-- movimiento (KeyPress "Down") (x, y) = (x, y-1)
-- movimiento _ (x, y) = (x, y)
