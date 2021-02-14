module Algoritmos (    
    camino, 
    contiguos, 
    filtro,
    rutaAOrdenes,
    obtenerCaminoValido
) where

import FrozenLake

lista = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]
lista2 = [(1,1),(1,2),(1,3), (1,4), (1,5),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]

-- quita los nodos sin salida de una trama DFS
obtenerCaminoValido :: [(Int, Int)] -> [(Int, Int)]
obtenerCaminoValido [] = []
obtenerCaminoValido lista
    | (camino lista []) == (init lista)  = lista
    | otherwise = obtenerCaminoValido caminoComputado
    where
        caminoComputado = (takeWhile (/=head(filtro lista)) lista) ++ filtro lista  ++ (dropWhile (/=head(restante)) lista)
        cam = camino lista []
        restante = drop (length cam) lista

-- devuelve el camino hasta encontrar un "salto" en la traza de DFS
camino :: [Posicion] -> [Posicion] -> [Posicion]
camino [] _ = []
camino (x:xs) cam
    | (length xs) < 1 = cam
    | contiguos x h = camino xs (cam++[x]) -- si son contiguos va añadiendo
    | otherwise = cam++[x] --
    where
        h = head xs

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

--
-- (takeWhile (/=head(filtro lista)) lista) ++ filtro lista  ++ (dropWhile (/=head(restante)) lista)
filtro :: [Posicion] -> [Posicion]
filtro [] = []
filtro lista = filtroAux cam restante (reverse busqueda)
    where
        cam = camino lista []
        restante = drop (length cam) lista
        busqueda = take (length cam) lista

filtroAux :: [t] -> [Posicion] -> [Posicion] -> [Posicion]
filtroAux [] [] [] = []
filtroAux cam restante (x:lista) = if (contiguos (head restante) x) then [x] else filtroAux cam restante lista

-- --------------------------------------------
-- Si esto es true el camino de la izq es el correcto  
--1 (camino [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,5),(5,5)] []) == (init [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,5),(5,5)])    

--si no
-- Darte un nuevo camino(cuando le pasas algo con nodos rotos)
-- (takeWhile (/=head(filtro lista)) lista) ++ filtro lista  ++ (dropWhile (/=head(restante)) lista)
-- Lo que retorne de vuelve a comparar. asi hasta que 2 sea true

-- filter (contiguos head(restante)) (reverse lista)

-- función que dado una ruta devuelve las órdenes necesarias para realizarla
-- camino a ordenes | camino | acumulador
rutaAOrdenes :: [Posicion] -> [Int] -> [Int]
rutaAOrdenes [] _  = []
rutaAOrdenes (x:xs) cam
    | (length xs) < 1 = cam
    | otherwise = rutaAOrdenes xs (cam++[traduceAInt x (head xs)])
    where
        h = head xs   

-- Función que pasa las órdenes a texto, útil para hacer debug
rutaAOrdenesTexto :: [Posicion] -> [[Char]] -> [[Char]]
rutaAOrdenesTexto [] _  = []
rutaAOrdenesTexto (x:xs) cam
    | (length xs) < 1 = cam
    | otherwise = rutaAOrdenesTexto xs (cam++[traduceATexto x (head xs)])
    where
        h = head xs

{-
Posibles movimientos
0 izquierda
1 abajo
2 derecha
3 arriba
["derecha","abajo","abajo","derecha","abajo","derecha","derecha"]
-}


-- Función que dada dos posiciones devuelve la orden en texto
-- para pasar de la primera a la segunda
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

-- Función que dada dos posiciones devuelve la orden
-- para pasar de la primera a la segunda
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
