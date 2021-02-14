module Intentos (
    camino, 
    contiguos, 
    filtro,
    caminoAOrdenes
) where

lista = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]

camino [] _ = []
camino (x:xs) cam
    | (length xs) < 1 = cam
    | contiguos x h = camino xs (cam++[x])
    | otherwise = cam++[x]
    where
        h = head xs


contiguos' a b 

contiguos actualPosc nextPosc
    | (abs (i1-i2)) <= 1 && (abs (j1-j2)) <= 1 = True
    | otherwise = False
    where
        i1 = fst actualPosc
        j1 = snd actualPosc
        i2 = fst nextPosc
        j2 = snd nextPosc

filtroAux [] [] [] = []
filtroAux cam restante (x:lista) = if (contiguos (head restante) x) then [x] else filtroAux cam restante lista

filtro [] = []
filtro lista = filtroAux cam restante (reverse busqueda)
    where
        cam = camino lista []
        restante = drop (length cam) lista
        busqueda = take (length cam) lista

-- (takeWhile (/=head(filtro lista)) lista) ++ filtro lista  ++ (dropWhile (/=head(restante)) lista)
-- (camino [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,5),(5,5)] []) == (init [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,5),(5,5)])    
-- filter (contiguos head(restante)) (reverse lista)

-- caminoAOrdenes [] _ _ = ([], [])
-- caminoAOrdenes (x:xs) cam debug
--     | (length xs) < 1 = (cam, debug)
--     | otherwise = caminoAOrdenes xs (cam++[traduce x (head xs)]) (debug++[(x, h)])
--     where
--         h = head xs

caminoAOrdenes [] _  = []
caminoAOrdenes (x:xs) cam
    | (length xs) < 1 = cam
    | otherwise = caminoAOrdenes xs (cam++[traduceMovimimentos x (head xs)])
      

{-
Posibles movimientos
0 izquierda
1 abajo
2 derecha
3 arriba
["derecha","abajo","abajo","derecha","abajo","derecha","derecha"]
-}

traduceMovimimentos actualPosc nextPosc
    | (i2-i1) == 1 = "abajo"
    | (i2-i1) == -1 = "arriba"
    | (j2-j1) == 1 = "derecha"
    | (j2-j1) == -1 = "izquierda"
    | otherwise = "F"
    where
        i1 = fst actualPosc
        j1 = snd actualPosc
        i2 = fst nextPosc
        j2 = snd nextPosc