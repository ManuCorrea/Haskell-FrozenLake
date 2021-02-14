module Algoritmos (    
    trazaDFS, 
    contiguos, 
    obtieneBifurcacion,
    rutaAOrdenes,
    obtenerCaminoValido
) where

import FrozenLake
--import I1M.Pila
import PilaConTipoDeDatoAlgebraico



{- 
Función que devuelve una lista con todas las posiciones de los caminos posibles.
 
Le pasamos como parámetros:
    * tablero -> Tablero válido creado (con solución).
    * posicion -> Posición inicial del tablero.
 
El resultado de la función es una lista de posiciones (caminos).
 
Para probar el método:
trazaDFS (fst (iniciaEntorno 5 123)) (snd (iniciaEntorno 5 123))
-}
trazaDFS :: Tablero -> Posicion -> [Posicion]
trazaDFS tablero posicion = trazaDFSAux tablero pilaInicial []
    where
        pilaInicial = apila posicion vacia

trazaDFSAux :: Tablero -> Pila Posicion -> [Posicion] -> [Posicion]
trazaDFSAux tablero casillasPosibles descubiertos
    | esVacia casillasPosibles = []
    | posEnPila (-1, -1) casillasPosibles = descubiertos
    | not ((cima casillasPosibles) `elem` descubiertos) = trazaDFSAux
                    tablero (iteraDirecciones tablero (desapila casillasPosibles) direcciones (row, column)) (descubiertos++[(cima casillasPosibles)]) -- for
    | otherwise = trazaDFSAux tablero (desapila casillasPosibles) descubiertos
    where
        row = fst (cima casillasPosibles)
        column = snd (cima casillasPosibles)

{- 
Función que devuelve un camino correcto quitando las ramas (posiciones) que no tienen salida.
 
Le pasamos como parámetros:
    * listaDFS -> Listado de trazas DFS (incluyendo las posiciones que no acaban en solución).
 
El resultado de la función es una lista de posiciones válidas.
 
Para probar el método:
lista1 = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
lista2 = [(1,1),(1,2),(1,3), (1,4),(1,5),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
obtenerCaminoValido lista1 ==> [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,5),(5,5)]
obtenerCaminoValido lista2 ==> [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,5),(5,5)]
-}
obtenerCaminoValido :: [Posicion] -> [Posicion]
obtenerCaminoValido [] = []
obtenerCaminoValido listaDFS
    | (camino listaDFS []) == (init listaDFS)  = listaDFS -- es contiguo hasta la meta
    | otherwise = obtenerCaminoValido caminoComputado
    where
        caminoComputado = (takeWhile (/=head(obtieneBifurcacion listaDFS)) listaDFS) ++ obtieneBifurcacion listaDFS  ++ (dropWhile (/=head(restante)) listaDFS)
        cam = camino listaDFS []
        restante = drop (length cam) listaDFS -- la lista borrando todos los contiguos



{- 
Función que devuelve el camino hasta encontrar un "salto" en la traza de DFS.
Salto es una posición que no es contigua con su próxima posición.
 
Le pasamos como parámetros:
    * xs -> Listado de trazas DFS (incluyendo las posiciones que no acaban en solución)
    * cam -> Acumulador.
 
El resultado de la función es una lista de posiciones válidas (camino válido).
 
Para probar el método:
lista1 = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
lista2 = [(1,1),(1,2),(1,3), (1,4),(1,5),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
camino lista1 [] ==> [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1)]
camino lista2 [] ==> [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1)]
-}
camino :: [Posicion] -> [Posicion] -> [Posicion]
camino [] _ = []
camino (x:xs) cam
    | (length xs) < 1 = cam
    | contiguos x h = camino xs (cam++[x]) -- si son contiguos va añadiendo
    | otherwise = cam++[x] --
    where
        h = head xs



{- 
Función que comprueba que dos posiciones son contiguas. No valen las diagonales.
Sólo compara las posiciones de arriba, abajo, izquierda y derecha.
 
Le pasamos como parámetros:
    * a -> Posición establecida.
    * b -> Posición a comparar.
 
El resultado de la función es True o False.
 
Para probar el método:
contiguos (2,2) (2,2) ==> False
contiguos (2,2) (1,2) ==> True
contiguos (2,2) (1,1) ==> False
-}
contiguos :: Posicion -> Posicion -> Bool
contiguos a b
    | (abs (x1-x2)) == 1 && (abs (y1-y2)) == 0 = True
    | (abs (x1-x2)) == 0 && (abs (y1-y2)) == 1 = True
    | otherwise = False
    where
        x1 = fst a
        y1 = snd a
        x2 = fst b
        y2 = snd b



{- 
Función que obtiene el nodo (posición) donde hay bifurcaciones.
 
Le pasamos como parámetros:
    * trazaDFS -> Lista con todas las posiciones de los caminos posibles, incluyendo los que no tienen solución.
 
El resultado de la función es un nodo (posición).
 
Para probar el método:
lista1 = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
lista2 = [(1,1),(1,2),(1,3), (1,4),(1,5),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
obtieneBifurcacion lista1 ==> [(4,4)]
obtieneBifurcacion lista2 ==> [(1,3)]
-}
obtieneBifurcacion :: [Posicion] -> [Posicion]
obtieneBifurcacion [] = [] --                        de derecha a izq
obtieneBifurcacion trazaDFS = obtieneBifurcacionAux cam restante (reverse busqueda)
    where
        cam = camino trazaDFS [] -- hace el camino DE LAS CONTIGUAS
        restante = drop (length cam) trazaDFS -- elimina todos los contiguos, PQ? porque le estamos pasando un camino en el que sabemos que hay corte
        busqueda = take (length cam) trazaDFS 

obtieneBifurcacionAux :: [t] -> [Posicion] -> [Posicion] -> [Posicion]
obtieneBifurcacionAux [] [] [] = []
obtieneBifurcacionAux cam restante (x:trazaDFS) = if (contiguos (head restante) x) then [x] else obtieneBifurcacionAux cam restante trazaDFS -- [x] es donde se va a juntar



{- 
Función que dado una ruta (itinerario o camino a seguir) devuelve las órdenes necesarias para realizarla.
 
Le pasamos como parámetros:
    * camino -> Lista de posiciones hasta encontrar un "salto".
    * cam -> Acumulador.
 
El resultado de la función es una lista con las ordenes a seguir para realizar el camino.
 
Para probar el método:
lista1 = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
lista2 = [(1,1),(1,2),(1,3), (1,4),(1,5),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
rutaAOrdenes (obtenerCaminoValido lista1) [] ==> [2,2,1,2,1,1,2,1]
rutaAOrdenes (obtenerCaminoValido lista2) [] ==> [2,2,1,2,1,1,2,1]
-}
rutaAOrdenes :: [Posicion] -> [Int] -> [Int]
rutaAOrdenes [] _  = []
rutaAOrdenes (x:xs) cam
    | (length xs) < 1 = cam
    | otherwise = rutaAOrdenes xs (cam++[traduceAInt x (head xs)])
    where
        h = head xs   



{- 
Función que pasa las órdenes a texto, muy útil para hacer debug.
 
Le pasamos como parámetros:
    * caminoValido -> Lista de posiciones que completan un camino válido.
    * cam -> Acumulador.
 
El resultado de la función es una lista con las ordenes a seguir (en texto) para realizar el camino.
 
Para probar el método:
lista1 = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
lista2 = [(1,1),(1,2),(1,3), (1,4),(1,5),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
rutaAOrdenesTexto (obtenerCaminoValido lista1) [] ==> ["derecha","derecha","abajo","derecha","abajo","abajo","derecha","abajo"]
rutaAOrdenesTexto (obtenerCaminoValido lista2) [] ==> ["derecha","derecha","abajo","derecha","abajo","abajo","derecha","abajo"]
-}
rutaAOrdenesTexto :: [Posicion] -> [[Char]] -> [[Char]]
rutaAOrdenesTexto [] _  = []
rutaAOrdenesTexto (x:xs) cam
    | (length xs) < 1 = cam
    | otherwise = rutaAOrdenesTexto xs (cam++[traduceATexto x (head xs)])
    where
        h = head xs



{- 
Función que dada dos posiciones devuelve la orden (en texto) para 
pasar de la primera posición a la segunda.

Le pasamos como parámetros:
    * posicion -> Posición en la que estamos.
    * posicion -> Posición a donde queremos movernos.

Los posibles movimientos son:
    * 0 izquierda
    * 1 abajo
    * 2 derecha
    * 3 arriba

El resultado de la función es un movimiento en texto.
 
Para probar el método:
traduceATexto (1,1) (1,2) ==> "derecha"
traduceATexto (2,4) (1,4) ==> "arriba"
-}
traduceATexto :: Posicion -> Posicion -> [Char]
traduceATexto a b
    | (f2-f1) == 1 = "abajo"
    | (f2-f1) == -1 = "arriba"
    | (c2-c1) == 1 = "derecha"
    | (c2-c1) == -1 = "izquierda"
    | otherwise = "noValido"
    where
        f1 = fst a
        c1 = snd a
        f2 = fst b
        c2 = snd b



{- 
Función que dada dos posiciones devuelve la orden (en número) para 
pasar de la primera posición a la segunda

Le pasamos como parámetros:
    * posicion -> Posición en la que estamos.
    * posicion -> Posición a donde queremos movernos.

Los posibles movimientos son:
    * 0 izquierda
    * 1 abajo
    * 2 derecha
    * 3 arriba

El resultado de la función es un movimiento en texto.
 
Para probar el método:
traduceAInt  (2,4) (1,4) ==> 3
traduceAInt  (1,1) (1,2) ==> 2
-}
traduceAInt :: Posicion -> Posicion -> Int
traduceAInt a b
    | (f2-f1) == 1 = 1
    | (f2-f1) == -1 = 3
    | (c2-c1) == 1 = 2
    | (c2-c1) == -1 = 0
    | otherwise = -1
    where
        f1 = fst a
        c1 = snd a
        f2 = fst b
        c2 = snd b
