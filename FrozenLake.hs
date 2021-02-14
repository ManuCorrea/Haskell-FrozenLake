module FrozenLake
(iniciaEntorno, paso, muestra, iniciaTablero, getTablero, getRecompensa, getFinalizado, Tablero, Posicion, Entorno, Accion, direcciones, posEnPila, iteraDirecciones) where

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
type Accion = Int

-- tupla de direcciones posibles
direcciones = [(1, 0), (0, 1), (-1, 0), (0, -1)]

----------------------------------------------------------------------
------------     Funciones Creación Entorno         ------------------
----------------------------------------------------------------------

-- Función principal que devuelve un entorno válido y el estado inicial en la salida (1,1)
-- n=Tamaño de la matrix cuadrada, semilla numero entero para generar numeros aleatorios
iniciaEntorno :: Int -> Int -> (Tablero, Posicion)
iniciaEntorno n semilla = (crearTablero n semilla, (1, 1))
-- iniciaEntorno 5 123

-- Función auxiliar que nos devuelve un tablero válido (que tiene solución)
-- n=Tamaño de la matrix cuadrada, semilla numero entero para generar numeros aleatorios
crearTablero :: Int -> Int -> Tablero
crearTablero n semilla
    | tableroValido (iniciaTablero n semilla) = iniciaTablero n semilla
    | otherwise = crearTablero n (semilla+1)


-- Función auxiliar que nos devuelve un tablero aleatorio
-- n=Tamaño de la matrix cuadrada, semilla numero entero para generar numeros aleatorios
-- Con un 70% de probabilidad casilla agujero/ 30% casilla transitable
iniciaTablero :: Int -> Int -> Tablero
iniciaTablero n semilla = M.fromList n n [if (y==1) then 'S' else if (y==n*n)then 'M' else (if (x>0.3) then 'A' else 'H') | (y,x) <- zip [1..(n*n)] (obtenerNumAleatorio semilla (n*n))]
-- iniciaTablero 5 123

-- Función auxiliar que nos devuelve un número aleatorio entre 0 y 1
-- semilla = semilla dada, n = cantidad de números aleatorios que quieres
obtenerNumAleatorio :: Int -> Int -> [Float]
obtenerNumAleatorio semilla n = take n $ drop (semilla*n) (randoms (mkStdGen 11) :: [Float])
-- obtenerNumAleatorio 1000 4 -> Obtiene 4 números con la "semilla"

{- 
Función que nos dice si un tablero generado es válido (que tiene solución).
    * Un tablero cualesquiera. 
El resultado de la función es True o False.
-}

tableroValido :: Tablero -> Bool
tableroValido tablero = tableroValidoAux tablero pilaInicial []
    where
        pilaInicial = apila (1,1) vacia
-- tableroValido $ iniciaTablero 5 110 -> True
-- tableroValido $ iniciaTablero 5 123 -> False

{- 
Función auxiliar que nos dice si un tablero generado es válido (que tiene solución).
 
Le pasamos como parámetros:
    * Un tablero cualesquiera.
    * Una pila vacía para almacenar movimientos.
    * Una lista(acumulador) para almacenar las posiciones que ya hemos pasado.
 
El resultado de la función es True o False.
-}
tableroValidoAux :: Tablero -> Pila Posicion -> [Posicion] -> Bool
tableroValidoAux tablero casillasPosibles descubiertos
    | esVacia casillasPosibles = False
    | posEnPila (-1, -1) casillasPosibles = True
    | not ((cima casillasPosibles) `elem` descubiertos) = tableroValidoAux
                    tablero (iteraDirecciones tablero (desapila casillasPosibles) direcciones (row, column)) (descubiertos++[(cima casillasPosibles)]) -- for

    | otherwise = tableroValidoAux tablero (desapila casillasPosibles) descubiertos
    where
        row = fst (cima casillasPosibles)
        column = snd (cima casillasPosibles)


{-
Función auxiliar que añade a la pila una posición transitable
En caso de llegar a la meta añade (-1,-1) indicando que ha finalizado correctamente.
-}
iteraDirecciones :: Tablero -> Pila (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Pila (Int, Int)
iteraDirecciones _ casillasPosibles [] _ = casillasPosibles --

iteraDirecciones tablero casillasPosibles (dir:direccioness) posActual
    | fueraTablero = iteraDirecciones tablero casillasPosibles direccioness posActual
    | tablero!newPos == 'M' = apila (-1, -1) casillasPosibles
    | tablero!newPos /= 'A'= iteraDirecciones tablero (apila newPos casillasPosibles) direccioness posActual
    | otherwise = iteraDirecciones tablero casillasPosibles direccioness posActual
        where
            newPos = (sum(map fst [posActual, dir]), sum(map snd [posActual, dir]))
            size = nrows tablero
            fueraTablero = (fst newPos) < 1 ||
                 (fst newPos) > size ||
                 (snd newPos) < 1 ||
                 (snd newPos) > size
-- iteraDirecciones (iniciaTablero 5 123) (vacia) direcciones (1,1)                 
            

-- Función para comprobar si una posición está en una pila de posiciones.
posEnPila :: Posicion -> Pila (Int, Int) -> Bool
posEnPila pos pilaPosiciones
    | esVacia pilaPosiciones = False
    | otherwise = pos == c || (posEnPila pos d)
    where c = cima pilaPosiciones
          d = desapila pilaPosiciones
-- posEnPila (1,1) vacia -> False
-- posEnPila (1,1) (apila (1,1) vacia) -> True

----------------------------------------------------------------------
------------     Funciones "Privadas" Interacción Entorno    ------------
----------------------------------------------------------------------

-- Función para obtener la recompensa del entorno
-- +1 si llegamos a la meta, 0 en caso contrario
recompensa :: Entorno -> Int
recompensa (tb, (x, y)) = case meta of
    'M' -> 1
    'A' -> -1
    _ -> 0
    where meta = tb!(fromIntegral(x), fromIntegral(y))
-- recompensa (iniciaEntorno 5 123) -> 0
-- recompensa ((iniciaTablero 5 123), (5,5)) -> 1
-- recompensa ((iniciaTablero 5 123), (2,1)) -> -1

-- Función que indica si llegamos a la meta o caemos en casilla agujero hemos terminado con el entorno
-- recibe entorno
finalizado :: Entorno -> Bool
finalizado (tb, (x, y)) = case meta of
    'M' -> True
    'A' -> True
    _ -> False
    where meta = tb!(fromIntegral(x), fromIntegral(y))
-- finalizado (iniciaEntorno 5 123) -> False
-- finalizado ((iniciaTablero 5 123), (5,5)) -> True
-- finalizado ((iniciaTablero 5 123), (2,1)) -> True

{-
Función que devuelve una posición tras aplicar una acción.
Parámentros acción y entorno.
Posibles movimientos:
0 izquierda - 1 abajo - 2 derecha - 3 arriba
-}

move :: Int -> Entorno -> Posicion
move 0 (_, (n, 1)) = (n, 1)
move 3 (_, (1, n)) = (1, n)
move accion (tb, (fila, columna)) = case accion of
    0 -> (fila, columna-1)
    1 -> (min (fila+1) (nrows tb), columna)
    2 -> (fila, min (columna+1) (nrows tb))
    3 -> (fila-1, columna)
    _ -> (fila, columna)
{-
move 0 ((iniciaTablero 5 123), (2,2))
(2,1)
move 1 ((iniciaTablero 5 123), (2,2))
(3,2)
move 2 ((iniciaTablero 5 123), (2,2))
(2,3)
move 3 ((iniciaTablero 5 123), (2,2))
(1,2)
-}
----------------------------------------------------------------------
------------     Funciones "Públicas" Interacción Entorno      -------
----------------------------------------------------------------------
-- Son las que el usuario va a usar para comunicarse con el entorno

-- retorna entornoComputado, recompensa, finalizado
paso :: Entorno -> Accion -> (Entorno, Int, Bool)
paso entorno accion =  (nuevoEntorno, recompensa nuevoEntorno, finalizado nuevoEntorno)
    where nuevoEntorno = (fst entorno, move accion entorno)
-- paso (iniciaEntorno  5 123) 1 -- paso hacia abajo
-- paso (iniciaEntorno  5 123) 2 -- paso hacia derecha

-- Función usada para imprimir el tablero (matriz indicando con X la posición actual)
muestra (tb, estado) = print (M.setElem 'X' estado tb)
-- muestra (iniciaEntorno  5 123)
-- muestra $ getTablero (paso (iniciaEntorno  5 123) 1)

-----   Funciones sobre la información retornada por paso, para extraer la información de la acción realizada     -----
-- Se usa para obtener el tablero al aplicar una acción
getTablero (a, _, _) = a

-- Se usa para obtener la recompensa al aplicar una acción al entorno
getRecompensa (_, a, _) = a
-- getRecompensa (paso (iniciaEntorno  5 123) 1) -> 0
-- getRecompensa (paso (iniciaEntorno  5 123) 2) -> 1

-- Se usa para saber si la acción aplicada hace que finalice el entorno
-- El entorno finaliza si se mueve a un Agujero o llega a la Meta
getFinalizado (_, _, a) = a
-- getFinalizado $ (paso (iniciaEntorno  5 123) 1) -> False
-- getFinalizado $ (paso (iniciaEntorno  5 123) 2) -> True
