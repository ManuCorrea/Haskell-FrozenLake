module FrozenLake (
    iniciaEntorno, paso, muestra, iniciaTablero, direcciones, posEnPila, iteraDirecciones,
    getTablero, getRecompensa, getFinalizado, 
    Tablero, Posicion, Entorno, Accion
) where


import Data.Array as A hiding ((!))
import Data.Matrix as M
import I1M.Pila
--import PilaConTipoDeDatoAlgebraico
import System.Random

{-
Vamos a modelar la cuadrícula con el tipo Matrix de la librería Data
En la cuadrícula vamos a tener:
    *H: Helado
    *S: Salida
    *M: Meta
    *A: Agujero

    Tendremos una variable Posicion con el que tendremos las
    coordenadas de la posición del agente.

    Recompensa +1 si alcanza la meta, en otro caso 0
    Se termina cuando el agente alcanza M.
-}

type Tablero = Matrix Char
type Posicion = (Int, Int)
type Entorno = (Tablero, Posicion)
type Accion = Int

-- Lista con las posibles direcciones posibles.
direcciones = [(1, 0), (0, 1), (-1, 0), (0, -1)]

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------     Funciones Creación Entorno         ----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{- 
Función que devuelve un entorno válido y el estado inicial en la salida (1,1)
 
Le pasamos como parámetros:
    * n -> Tamaño de la matriz cuadrada.
    * semilla -> Número entero con el que generamos números aleatorios.
 
El resultado de la función es un tablero válido con la posición en la que te encuentras.

Para probar el método:
iniciaEntorno 5 123
-}
iniciaEntorno :: Int -> Int -> (Tablero, Posicion)
iniciaEntorno n semilla = (crearTablero n semilla, (1, 1))



{-
Función que nos devuelve un tablero válido (que tiene solución).

Le pasamos como parámetros:
    * n -> Tamaño de la matriz cuadrada.
    * semilla -> Número entero con el que generamos números aleatorios.

El resultado de la función es un tablero válido.

Para probar el método:
crearTablero 5 123
-}
crearTablero :: Int -> Int -> Tablero
crearTablero n semilla
    | tableroValido (iniciaTablero n semilla) = iniciaTablero n semilla
    | otherwise = crearTablero n (semilla+1)



{-
Función que nos devuelve un tablero aleatorio (con o sin solución), con un 
70% de probabilidad de agujero 'A' y un 30% de casilla transitable 'H'.

Le pasamos como parámetros:
    * n -> Tamaño de la matriz cuadrada.
    * semilla -> Número entero con el que generamos números aleatorios.

El resultado de la función es un tablero aleatorio.

Para probar el método:
iniciaTablero 5 123
-}
iniciaTablero :: Int -> Int -> Tablero
iniciaTablero n semilla = M.fromList n n [if (y==1) then 'S' else if (y==n*n)then 'M' else (if (x>0.3) then 'A' else 'H') | (y,x) <- zip [1..(n*n)] (obtenerNumAleatorio semilla (n*n))]



{-
Función que nos devuelve números aleatorio entre 0 y 1.

Le pasamos como parámetros:
    * semilla -> Número entero con el que generamos números aleatorios.
    * n -> Cantidad de números aleatorios que quieremos.

El resultado de la función es una lista números aleatorio.

Para probar el método : 
obtenerNumAleatorio 1000 4
-}
obtenerNumAleatorio :: Int -> Int -> [Float]
obtenerNumAleatorio semilla n = take n $ drop (semilla*n) (randoms (mkStdGen 11) :: [Float])



{- 
Función que nos dice si un tablero generado es válido (que tiene solución).

Le pasamos como parámetros:
    * tablero -> Un tablero cualesquiera. 
    
El resultado de la función es True o False.

Para probar el método:
tableroValido $ iniciaTablero 5 110 ==> True
tableroValido $ iniciaTablero 5 123 ==> False
-}
tableroValido :: Tablero -> Bool
tableroValido tablero = tableroValidoAux tablero pilaInicial []
    where pilaInicial = apila (1,1) vacia

{- 
Función auxiliar que nos dice si un tablero generado es válido (que tiene solución).
 
Le pasamos como parámetros:
    * tablero -> Un tablero cualesquiera.
    * casillasPosibles -> Pila que contiene las posiciones a las que el agente puede moverse.
    * descubiertos -> Una lista(acumulador) para almacenar las posiciones que ya hemos pasado.
 
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
Función que añade a la pila una posición transitable, es decir, una posición a la que podemos acceder.
En caso de llegar a la meta añade (-1,-1) indicando que ha finalizado correctamente.

Le pasamos como parámetros:
    * tablero -> Un tablero.
    * casillasPosibles -> Pila que contiene las posiciones a las que el agente puede moverse.
    * direcciones -> Lista de posibles movimientos.

El resultado es la siguiente posición correcta, a la que pueda moverse, añadida en la pila.

Para probar el método:
iteraDirecciones (iniciaTablero 5 123) (vacia) direcciones (1,1)  ==> (1,2)|-
-}
iteraDirecciones :: Tablero -> Pila (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Pila (Int, Int)
iteraDirecciones _ casillasPosibles [] _ = casillasPosibles

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
               
            

{-
Función que comprueba si una posición está en una pila de posiciones.

Le pasamos como parámetros:
    * pos -> Una posición concreta.
    * casillasPosibles -> Pila que contiene las posiciones a las que el agente puede moverse.

El resultado de la función es True o False.

Para probar el método:
posEnPila (1,1) vacia ==> False
posEnPila (1,1) (apila (1,1) vacia) ==> True
-}
posEnPila :: Posicion -> Pila (Int, Int) -> Bool
posEnPila pos pilaPosiciones
    | esVacia pilaPosiciones = False
    | otherwise = pos == c || (posEnPila pos d)
    where c = cima pilaPosiciones
          d = desapila pilaPosiciones



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------     Funciones "Privadas" Interacción Entorno        ---------------------------------------------------------------
----------------------------------------------------------            (No las ve el usuario)                   ---------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Función que obtiene la recompensa del entorno jugado.
Si llegamos a la meta (juego completado) obtenemos 1 punto, en caso contrario 0.

Le pasamos como parámetros:
    * entorno -> Un entorno cualesquiera.

El resultado de la función es:
    * 1 -> Se ha comletado el juego llegabdno a la meta.
    * 0 -> No hemos acabado el juego.
    * -1 -> Hemos caído en un agujero (casilla 'A').

Para probar el método:
recompensa (iniciaEntorno 5 123) ==> 0
recompensa ((iniciaTablero 5 123), (5,5)) ==> 1
recompensa ((iniciaTablero 5 123), (2,1)) ==> -1
-}
recompensa :: Entorno -> Int
recompensa (tb, (x, y)) = case meta of
    'M' -> 1
    'A' -> -1
    _ -> 0
    where meta = tb!(fromIntegral(x), fromIntegral(y))



{-
Función que indica si hemos llegado a la meta (juego acabdo satisfactoriamente).

Le pasamos como parámetros:
    * entorno -> Un entorno cualesquiera.

El resultado de la función es True o False.

Para probar el método:
finalizado (iniciaEntorno 5 123) ==> False
finalizado ((iniciaTablero 5 123), (5,5)) ==> True
finalizado ((iniciaTablero 5 123), (2,1)) ==> True
-}
finalizado :: Entorno -> Bool
finalizado (tb, (x, y)) = case meta of
    'M' -> True
    'A' -> True
    _ -> False
    where meta = tb!(fromIntegral(x), fromIntegral(y))



{-
Función que devuelve una nueva posición tras aplicar una acción dada.

Le pasamos como parámetros:
    * entorno -> Un entorno cualesquiera
    * accion -> Accion que queremos ejecutar

Los posibles movimientos son :
0 izquierda - 1 abajo - 2 derecha - 3 arriba

Para probar el método:
mover 0 ((iniciaTablero 5 123), (2,2)) ==> (2,1)
mover 1 ((iniciaTablero 5 123), (2,2)) ==> (3,2)
mover 2 ((iniciaTablero 5 123), (2,2)) ==> (2,3)
mover 3 ((iniciaTablero 5 123), (2,2)) ==> (1,2)
-}
mover :: Int -> Entorno -> Posicion
mover 0 (_, (n, 1)) = (n, 1)
mover 3 (_, (1, n)) = (1, n)
mover accion (tb, (fila, columna)) = case accion of
    0 -> (fila, columna-1)
    1 -> (min (fila+1) (nrows tb), columna)
    2 -> (fila, min (columna+1) (nrows tb))
    3 -> (fila-1, columna)
    _ -> (fila, columna)



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------        Funciones "Públicas" Interacción Entorno           ------------------------------------------------------------
-------------------------------------------------------   (Las usa el usuario para comunicarse con el entrono)    ------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Función que devuelve un entorno, la nueva posición tras aplicar una acción dada,
la recompensa (que indica si ha finalizado o no) y si el entorno es finalizado.

Le pasamos como parámetros:
    * entorno -> Un entorno cualesquiera
    * accion -> Accion que queremos ejecutar

Para probar el método:
paso (iniciaEntorno  5 123) 1 ==> paso hacia abajo
paso (iniciaEntorno  5 123) 2 ==> paso hacia derecha
-}
paso :: Entorno -> Accion -> (Entorno, Int, Bool)
paso entorno accion =  (nuevoEntorno, recompensa nuevoEntorno, finalizado nuevoEntorno)
    where nuevoEntorno = (fst entorno, mover accion entorno)



{-
Función que imprime el tablero, indicando con 'X' la posición actual
donde se encuentra el agente.

Le pasamos como parámetros:
    * entorno -> Un entorno cualesquiera

Para probar el método:
-- muestra (iniciaEntorno  5 123)
-- muestra $ getTablero (paso (iniciaEntorno  5 123) 1)
-}
muestra :: (Tablero, Posicion) -> IO ()
muestra (tb, estado) = print (M.setElem 'X' estado tb)



-------------------------------------   Funciones sobre la información retornada por paso, para extraer la información de la acción realizada   -------------------------------------

{-
Función que se usa para obtener el tablero al aplicar una acción.

Para probar el método:
-}
getTablero (a, _, _) = a



{-
Función que se usa para obtener la recompensa al aplicar una acción al entorno.

Para probar el método:
getRecompensa (paso (iniciaEntorno  5 123) 1) ==> 0
getRecompensa (paso (iniciaEntorno  5 123) 2) ==> 1
-}
getRecompensa (_, a, _) = a



{-
Función que se usa para saber si la acción aplicada hace que finalice el entorno.
El entorno finaliza si se mueve a un Agujero o llega a la Meta.

Para probar el método:
getFinalizado $ (paso (iniciaEntorno  5 123) 1) ==> False
-- getFinalizado $ (paso (iniciaEntorno  5 123) 2) ==> True
-}
getFinalizado (_, _, a) = a

