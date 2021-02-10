import Data.Array as A hiding ((!))
import Data.Matrix as M
--import Data.Vector as V

import I1M.Pila

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

-- Genera un tablero base con una salida y una meta
-- TODO generar

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

--take 5 $ randoms (mkStdGen 11) :: [Float]

-- TODO que se creen H o A de forma aleatoria
iniciaTablero :: (Num a) => a -> Tablero
iniciaTablero n = matrix n n (\(i, j) -> if (i==1 && j==1) then 'S'
                                         else if (i == n && j == n) then 'M'
                                         else  'H')
                                         where
                                            n = (fromIntegral n)

iniciaTabler :: (Num a, Integral a) => a -> Tablero
iniciaTabler n = matrix nX nX (\(i, j) -> if (i==1 && j==1) then 'S'
                                         else if (i == nX && j == nX) then 'M'
                                         else  'H')
                                         where
                                            nX = fromIntegral n  

iniciaTablerRandom n = matrix nX nX (\(i, j) -> if (i==1 && j==1) then 'S'
                                         else if (i == nX && j == nX) then 'M'
                                         else 
                                            if (0.5 > 0.8) then 'H' else 'A')
                                         where
                                            nX = fromIntegral n                                                                     

--iteraDirecciones :: (Num a, Ord a, Num b, Num c, Num d) => Tablero -> [(a, a)] -> [(b, b)] -> c -> c -> [(d, d)]

nFilasColumnas = 5
tb = iniciaTabler nFilasColumnas -- para realizar tests
directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

iteraDirecciones _ frontera [] _ _ = frontera ++ [(-2,-2)]

iteraDirecciones tablero frontera (dir:directionss) r c
    | fueraTablero = iteraDirecciones tablero frontera directionss r c
    | tablero!(r_new, c_new) == 'M' = [(-1, -1)] --return True Mapa Valido
    | tablero!(r_new, c_new) /= 'A'= iteraDirecciones tablero (frontera ++ [(r_new, c_new)]) directionss r c --frontier.append((r_new, c_new))
    | otherwise = iteraDirecciones tablero frontera directionss r c
        where
            r_new = fromIntegral(r + fst dir)
            c_new = fromIntegral(c + snd dir)
            r_newC = fromIntegral(r + fst dir)
            c_newC = fromIntegral(c + snd dir)
            size = nrows tablero
            fueraTablero = r_newC < 1 ||
                 r_newC >= size ||
                 c_newC < 1 ||
                 c_newC >= size
--iteraDirecciones tb [(1,1)] directions 2 1 da [(1,1),(1,1),(-2,-2)]

{-
tableroValidoAux :: Tablero -> [(Int, Int)] -> [(Int, Int)] -> Bool
tableroValidoAux tablero [] [] = tableroValidoAux tablero [(0, 0)] []

tableroValidoAux tablero frontera descubiertos
    | (length frontera > 0) = explore
    | (length frontera == 0) = False
        where explore -- while en linea 46
            | !(fronteraPop `elem` descubiertos) =
                iteraDirecciones tablero frontera directions (descubiertos++ [fronteraPop]) -- for
                -- vuelve a hacer el bucle
                tableroValidoAux tablero frontera directions descubiertos
            | otherwise =  False
            where fronteraPop = (1,1)


tableroValido :: Tablero -> Bool
tableroValido tablero = 
-}

state = (1, 1) -- we start in 'S'
{-
0 LEFT
1 DOWN
2 RIGHT
3 UP
-}

observation = undefined
-- +1 si llegamos a la meta, 0 en caso contrario
reward (x, y) = if (tb!(fromIntegral(x), fromIntegral(y)) == 'M') then 1.0 else 0
-- si llegamos a la meta hemos terminado con el entorno
done (x, y) = if (tb!(fromIntegral(x), fromIntegral(y)) == 'M') then True else False


-- FIXME error de tipos o hacer funcion a parte para comprobar si es válido
{-
move action (fila, columna)
    | action == 0 = (fila, leftCol)
    | action == 1 = (downFil, columna)
    | action == 2 = (fila, min(columna+1 nFilasColumnas))
    | action == 3 = (max((fila-1) 1), columna)
    | otherwise = (fila, columna)
    where
        leftCol = max(columna-1 1)
        downFil = min(fila+1 nFilasColumnas)
-}
-- no es seguro
move' action (fila, columna)
    | action == 0 = (fila, columna-1)
    | action == 1 = (fila+1, columna)
    | action == 2 = (fila, columna+1)
    | action == 3 = (fila-1, columna)
    | otherwise = (fila, columna)

-- retorna info del entorno observation, reward, done, info
step action = undefined

