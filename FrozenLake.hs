module FrozenLake
(iniciaEntorno, step, muestra) where

import Data.Array as A hiding ((!))
import Data.Matrix as M

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
type Posicion = (Int, Int)
type Entorno = (Tablero, Posicion)
type Action = Int

getRandomNum = randomRIO (0, 1) :: IO Float -- usar asignando num <- getRandomNum

obtenerNumAleatorio idx n = take n $ drop (idx*n) (randoms (mkStdGen 11) :: [Float])

iniciaTablero :: Int -> Int -> Tablero
iniciaTablero n idx = M.fromList n n [if (y==1) then 'S' else if (y==n*n)then 'M' else (if (x>0.3) then 'A' else 'H') | (y,x) <- zip [1..(n*n)] (obtenerNumAleatorio idx (n*n))]

--crearTablero :: Int -> Int -> Tablero
crearTablero n idx
    | tableroValido (iniciaTablero n idx) = iniciaTablero n idx
    | otherwise = crearTablero n (idx+1)

--iteraDirecciones :: (Num a, Ord a, Num b, Num c, Num d) => Tablero -> [(a, a)] -> [(b, b)] -> c -> c -> [(d, d)]

nFilasColumnas = 5
tb = iniciaTablero nFilasColumnas 0 -- para realizar tests

-- iteraDirecciones :: (Integral a) => Matrix Char
--     -> Pila (Int, Int) -> [(a, a)] -> a -> a -> Pila (Int, Int)
-- hemos terminado el for sobre direcciones
iteraDirecciones _ casillasPosibles [] _ = casillasPosibles --

iteraDirecciones tablero casillasPosibles (dir:directionss) posActual
    | fueraTablero = iteraDirecciones tablero casillasPosibles directionss posActual
    | tablero!newPos == 'M' = apila (-1, -1) casillasPosibles --return True Mapa Valido
    | tablero!newPos /= 'A'= iteraDirecciones tablero (apila newPos casillasPosibles) directionss posActual --frontier.append((r_new, c_new))
    | otherwise = iteraDirecciones tablero casillasPosibles directionss posActual
        where
            newPos = (sum(map fst [posActual, dir]), sum(map snd [posActual, dir]))
            size = nrows tablero
            fueraTablero = (fst newPos) < 1 ||
                 (fst newPos) > size ||
                 (snd newPos) < 1 ||
                 (snd newPos) > size
--iteraDirecciones tb [(1,1)] directions 2 1 da [(1,1),(1,1),(-2,-2)]

directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

tableroValidoAux :: Tablero -> Pila Posicion -> [Posicion] -> Bool

tableroValidoAux tablero casillasPosibles descubiertos
    | esVacia casillasPosibles = False
    | pertenecePila (-1, -1) casillasPosibles = True
    | not ((cima casillasPosibles) `elem` descubiertos) = tableroValidoAux
                    tablero (iteraDirecciones tablero (desapila casillasPosibles) directions (row, column)) (descubiertos++[(cima casillasPosibles)]) -- for

    | otherwise = tableroValidoAux tablero (desapila casillasPosibles) descubiertos
    where
        row = fst (cima casillasPosibles)
        column = snd (cima casillasPosibles)
            

pertenecePila :: (Eq a) => a -> Pila a -> Bool
pertenecePila y pila
    | esVacia pila = False
    | otherwise = y == c || (pertenecePila y d)
    where c = cima pila
          d = desapila pila


tableroValido :: Tablero -> Bool
                        --              tablero   posibles   descubiertos
tableroValido tablero = tableroValidoAux tablero pilaInicial []
    where
        pilaInicial = apila (1,1) vacia

observation = undefined
-- +1 si llegamos a la meta, 0 en caso contrario
reward (x, y) = case meta of
    'M' -> 1.0
    _ -> 0
    where meta = tb!(fromIntegral(x), fromIntegral(y))
-- si llegamos a la meta hemos terminado con el entorno
done (x, y) = case meta of
    'M' -> True
    _ -> False
    where meta = tb!(fromIntegral(x), fromIntegral(y))

-- FIXME error de tipos o hacer funcion a parte para comprobar si es válido

-- move action (fila, columna)
--     | action == 0 = (fila, leftCol)
--     | action == 1 = (downFil, columna)
--     | action == 2 = (fila, min(columna+1 nFilasColumnas))
--     | action == 3 = (max((fila-1) 1), columna)
--     | otherwise = (fila, columna)
--     where
--         leftCol = max(columna-1 1)
--         downFil = min(fila+1 nFilasColumnas)

{-
0 LEFT
1 DOWN
2 RIGHT
3 UP
-}

-- no es seguro
move :: Int -> Entorno -> Posicion
move 0 (_, (n, 1)) = (n, 1)
-- move 1 (tb, (x, n)) = (x, n)
--     where x = nrows tb
move 3 (_, (1, n)) = (1, n)
move action (tb, (fila, columna)) = case action of
    0 -> (fila, columna-1)
    1 -> (min (fila+1) (nrows tb), columna)
    2 -> (fila, min (columna+1) (nrows tb))
    3 -> (fila-1, columna)
    _ -> (fila, columna)

-- retorna info del entorno observation, reward, done, info
step :: Action -> Posicion
step action =  undefined
--iniciaEntorno :: Int -> (Tablero, Posicion)
-- devolvemos un tablero iniciado válido y el estado inicial en la meta
iniciaEntorno n semilla = (crearTablero n semilla, (1, 1))

muestra (tb, estado) = print (M.setElem 'X' estado tb)

