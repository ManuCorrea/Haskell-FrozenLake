-- Trabajo Final Programación Declarativa.

-- Desarrollado por los alumnos:
-- Manuel Correa Gomez
-- Alejandro Jiménez Martín

-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe 
import System.IO 
import System.Environment (getArgs)
import Text.Printf

import I1M.Pila
import Data.Matrix as M

import FrozenLake
import Intentos

type Tablero = Matrix Char
type Posicion = (Int, Int)
type Entorno = (Tablero, Posicion)
type Action = Int

juego  = do
    lineaBlanco
    putStrLn "Introduce el tamaño del tablero:"
    x <- getLine
    putStrLn "Introduce una semilla:"
    y <- getLine
    
    let n = read x :: Int
    let idx = read y :: Int 
    lineaBlanco
    let guardar = iniciaEntorno n idx
    --guardar <- iniciaEntorno n idx
    muestra (guardar)
    
    let x = getDone (step guardar 1)
    if (not x) then do
        let guardar = getEntorno (step guardar 1)
        muestra guardar
    else
        print "FFFF"

    print "a"
    -- let guardar = step x 0
    -- muestra (guardar)
    --iniciaEntorno n idx
    --muestra entorno
    {-
    funcion [x:todasOrdenes] entorno = funcion todasordenes step(entorno x) 
    si resbala, = funcion (volver a calcular ordenes) entorno
    otherwise = funcion todasordenes step(entorno x) 
    
    -}

lineaBlanco = do
    putChar '\n'

iteraDirecciones :: Tablero -> Pila (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Pila (Int, Int)
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

directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]
--                                                             Tiene que devolver lista con ordenes
resuelveTableroAux :: Tablero -> Pila Posicion -> [Posicion] -> [(Int, Int)]
resuelveTableroAux tablero casillasPosibles descubiertos
    | esVacia casillasPosibles = []
    | pertenecePila (-1, -1) casillasPosibles = descubiertos
    | not ((cima casillasPosibles) `elem` descubiertos) = resuelveTableroAux
                    tablero (iteraDirecciones tablero (desapila casillasPosibles) directions (row, column)) (descubiertos++[(cima casillasPosibles)]) -- for

    | otherwise = resuelveTableroAux tablero (desapila casillasPosibles) descubiertos
    where
        row = fst (cima casillasPosibles)
        column = snd (cima casillasPosibles)
            

pertenecePila :: Posicion -> Pila (Int, Int) -> Bool
pertenecePila y pila
    | esVacia pila = False
    | otherwise = y == c || (pertenecePila y d)
    where c = cima pila
          d = desapila pila


-- DFS
resuelveTablero :: Tablero -> (Int, Int) -> [(Int, Int)]
                        --              tablero   posibles   descubiertos
resuelveTablero tablero posicion = resuelveTableroAux tablero pilaInicial []
    where
        pilaInicial = apila posicion vacia

--                                                          camino []
-- aplicaOrdenesAux (iniciaEntorno 5 123) (caminoAOrdenes (obtenerCaminoValido fst((iniciaEntorno 5 123))) [])
-- aplicaOrdenesAux (iniciaEntorno 5 123) [1,2,2,1,1,1,2,2]
aplicaOrdenesAux entorno [] = lineaBlanco
aplicaOrdenesAux entorno (orden:ordenes) = do
    muestra (getEntorno (step entorno orden))
    let done = (getDone (step entorno orden))
    if done then
        do
            let reward = (getReward (step entorno orden))
            print reward
            if (reward>0) then do
                print "campeón has ganao ;)"
                else do
                aplicaOrdenesAux entorno []
                print "Perdedor te caiste"
        else do
        print done
        aplicaOrdenesAux (getEntorno (step entorno orden)) ordenes
    
aplicaOrdenes entorno = aplicaOrdenesAux entorno (caminoAOrdenes (obtenerCaminoValido (listaDFS ++ meta)) [])
    where
        listaDFS = resuelveTablero (fst entorno) (snd entorno)
        meta = [(tamano, tamano)]
        tamano = nrows (fst entorno) 


--aplicaOrdenes entorno (orden:ordenes) = aplicaOrdenesAux


