module Mapa ( 
    obtenerNumAleatorio,
    crearTablero,
    iniciaTablero,
    iteraDirecciones,
    --direcciones,
    tableroValido,
    move
) where 

import Data.Array as A hiding ((!))
import Data.Matrix as M
import Data.List 
import System.Random
import PilaConTipoDeDatoAlgebraico

type Tablero = Matrix Char
type Posicion = (Int, Int)
type Entorno = (Tablero, Posicion)
type Action = Int


obtenerNumAleatorio idx n = take n $ drop (idx*n) (randoms (mkStdGen 11) :: [Float])

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

crearTablero n idx
    | tableroValido (iniciaTablero n idx) = crearTablero n idx
    | otherwise = crearTablero n (idx + 1)

iniciaTablero n idx = M.fromList n n [if (y==1) then 'S' else if (y==n*n)then 'M' else (if (x>0.8) then 'A' else 'H') | (y,x) <- zip [1..(n*n)] (obtenerNumAleatorio idx (n*n))]

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

iteraDirecciones _ casillasPosibles [] _ = casillasPosibles --
iteraDirecciones tablero casillasPosibles (dir:direccioness) actualPosc
    | fueraTablero = iteraDirecciones tablero casillasPosibles direccioness actualPosc
    | tablero!newPosc == 'M' = apila (-1,-1) casillasPosibles --return True Mapa válido
    | tablero!newPosc /= 'A' = iteraDirecciones tablero (apila newPosc casillasPosibles) direccioness actualPosc --frontier.append((r_new, c_new))
    | otherwise = iteraDirecciones tablero casillasPosibles direccioness actualPosc
    where newPosc = (sum(map fst [actualPosc, dir]), sum(map snd [actualPosc, dir]))
          sizeMatrix = nrows tablero
          fueraTablero = (fst newPosc) < 1 || (fst newPosc) > sizeMatrix || (snd newPosc) < 1 || (snd newPosc) > sizeMatrix

direcciones = [(1, 0), (0, 1), (-1, 0), (0, -1)]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
esCasillaPosible y casillasPosibles
    | esVacia casillasPosibles = False
    | otherwise = y == c || (esCasillaPosible y d)
    where c = cima casillasPosibles
          d = desapila casillasPosibles

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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