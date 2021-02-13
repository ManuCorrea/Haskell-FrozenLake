lista = [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),(4,3),(4,2),(5,2),(5,1),(4,5),(5,5)]

camino [] _ = []
camino (x:xs) cam
    | (length xs) < 1 = cam
    | contiguos x h = camino xs (cam++[x])
    | otherwise = cam++[x]
    where
        h = head xs

contiguos a b
    | (abs (x1-x2)) <= 1 && (abs (y1-y2)) <= 1 = True
    | otherwise = False
    where
        x1 = fst a
        y1 = snd a
        x2 = fst b
        y2 = snd b

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

caminoAOrdenes [] _ _ = ([], [])
caminoAOrdenes (x:xs) cam debug
    | (length xs) < 1 = (cam, debug)
    | otherwise = caminoAOrdenes xs (cam++[traduce x (head xs)]) (debug++[(x, h)])
    where
        h = head xs

caminoAOrdenes' [] _  = []
caminoAOrdenes' (x:xs) cam
    | (length xs) < 1 = cam
    | otherwise = caminoAOrdenes' xs (cam++[traduce x (head xs)])
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

traduce a b
    | (f2-f1) == 1 = "abajo"
    | (f2-f1) == -1 = "arriba"
    | (c2-c1) == 1 = "derecha"
    | (c2-c1) == -1 = "izquierda"
    | otherwise = "F"
    where
        f1 = fst a
        c1 = snd a
        f2 = fst b
        c2 = snd b