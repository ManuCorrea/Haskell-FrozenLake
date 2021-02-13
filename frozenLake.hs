import Data.Array as A hiding ((!))
import Data.Matrix as M
import Data.List

import PilaConTipoDeDatoAlgebraico

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
type Posicion = (Int, Int)
type Entorno = (Tablero, Posicion)
type Action = Int


obtenerNumAleatorio idx n = take n $ drop (idx*n) (randoms (mkStdGen 11) :: [Float])


iniciaTablero n idx = M.fromList n n [if (y==1) then 'S' else if (y==n*n)then 'M' else (if (x>0.8) then 'A' else 'H') | (y,x) <- zip [1..(n*n)] (obtenerNumAleatorio idx (n*n))]


crearTablero n idx
    | tableroValido (iniciaTablero n idx) = crearTablero n idx
    | otherwise = crearTablero n (idx + 1)


-- hemos terminado el for sobre direcciones
iteraDirecciones _ casillasPosibles [] _ = casillasPosibles --

iteraDirecciones tablero casillasPosibles (dir:direccioness) actualPosc
    | fueraTablero = iteraDirecciones tablero casillasPosibles direccioness actualPosc
    | tablero!newPosc == 'M' = apila (-1,-1) casillasPosibles --return True Mapa válido
    | tablero!newPosc /= 'A' = iteraDirecciones tablero (apila newPosc casillasPosibles) direccioness actualPosc --frontier.append((r_new, c_new))
    | otherwise = iteraDirecciones tablero casillasPosibles direccioness actualPosc
    where newPosc = (sum(map fst [actualPosc, dir]), sum(map snd [actualPosc, dir]))
          sizeMatrix = nrows tablero
          fueraTablero = (fst newPosc) < 1 || (fst newPosc) > sizeMatrix || (snd newPosc) < 1 || (snd newPosc) > sizeMatrix 

--iteraDirecciones tb [(1,1)] directions 2 1 da [(1,1),(1,1),(-2,-2)]


direcciones = [(1, 0), (0, 1), (-1, 0), (0, -1)]


tableroValido :: Tablero -> Bool
                        --              tablero   posibles   descubiertos
tableroValido tablero = tableroValidoAux tablero casillasPosibles []
    where casillasPosibles = apila (1,1) vacia

tableroValidoAux tablero casillasPosibles descubiertos
    | esVacia casillasPosibles = False
    | esCasillaPosible (-1, -1) casillasPosibles = True
    | not ((cima casillasPosibles) `elem` descubiertos) = 
        tableroValidoAux tablero (iteraDirecciones tablero (desapila casillasPosibles) direcciones posc) (descubiertos ++ [(cima casillasPosibles)]) -- for
    | otherwise = tableroValidoAux tablero (desapila casillasPosibles) descubiertos
    where posc = (fst (cima casillasPosibles), snd (cima casillasPosibles))
        

esCasillaPosible :: (Eq a) => a -> Pila a -> Bool
esCasillaPosible y pila
    | esVacia pila = False
    | otherwise = y == c || (esCasillaPosible y d)
    where c = cima pila
          d = desapila pila

-----------------------------------------------------------------------------------------------------------------------------------------


-- Calculamos el porcentaje estableciendo siempre el 20%
porcentaje n = fromIntegral $ round ((x*20)/100) 
    where x = n*n 


-- Método que calcula si es frontera, es decir, los bordes de la matriz

-- Para probar -> esFrontera (iniciaTablero 8 234)
esFrontera m = nub [(i,j) | i <- [1..numF], j <- [1..numC], i==1 || j==1] ++ [(i,j) | i <- [1..numF], j <- [1..numC], i==numF || j==numC]
    where numF = nrows m
          numC = ncols m 

-- Para probar -> esFrontera2 (iniciaTablero 8 234)
esFrontera2 m = [(i,j) | ((i,j),_) <- indexs m, i==1 || j==1] ++ [(i,j) | ((i,j),_) <- indexs m, i==numF || j==numC]
    where numF = nrows m
          numC = ncols m 

-- Para probar -> indexs (iniciaTablero 8 234)
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


-- Valores para hacer pruebas
nFilasColumnas = 5
tb = iniciaTablero nFilasColumnas 0 -- para realizar tests

-- le paso una matriz y un estado y me devuelve la matriz original con el estado mosdificado
cambiaEstado tablero pos = print (M.setElem 'X' pos tb)


-- Meter getRandom en una lista y obtener head.
getRandomNum = randomRIO (0, 1) :: IO Float -- usar asignando num <- getRandomNum
randomNum = head [randRio]
    where randRio = randomRIO (0, 1) :: IO Float

-- Probar la función action con 4 guardas.
move :: Int -> Entorno -> Posicion
move 0 (_, (n, 1)) = (n, 1)
move 3 (_, (1, n)) = (1, n)
move action (tb, (i, j)) = case action of
    0 -> (i, j-1)
    1 -> (min (i+1) size, j)
    2 -> (i, min (j+1) size)
    3 -> (i-1, j)
    where size = nrows tb 

{-
0 LEFT
1 DOWN
2 RIGHT
3 UP
-}