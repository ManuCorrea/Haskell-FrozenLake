-- Trabajo Final Programación Declarativa.

-- Desarrollado por los alumnos:
-- Manuel Correa Gomez
-- Alejandro Jiménez Martín

-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
import I1M.Pila
import Data.Matrix as M

import FrozenLake
import Algoritmos

-- función principal que hemos creado para resolver el juego
-- mostrando el camino realizado por el algoritmo 
resuelveJuego :: Entorno -> IO ()
resuelveJuego entorno = resuelveJuegoAux entorno (rutaAOrdenes (obtenerCaminoValido (listaDFS ++ meta)) [])
    where
        listaDFS = trazaDFS (fst entorno) (snd entorno) -- obtiene las soluciones
        meta = [(tamano, tamano)]
        tamano = nrows (fst entorno) 

-- resuelveJuego (iniciaEntorno 5 123)
--resuelveJuego entorno (orden:ordenes) = resuelveJuegoAux

-- resuelveJuegoAux (iniciaEntorno 5 123) (rutaAOrdenes (obtenerCaminoValido fst((iniciaEntorno 5 123))) [])
-- resuelveJuegoAux (iniciaEntorno 5 123) [1,2,2,1,1,1,2,2]
-- Se le pasa entorno y la lista de acciones que han sido computadas en resuelveJuego
resuelveJuegoAux :: Entorno -> [Accion] -> IO ()
resuelveJuegoAux entorno [] = lineaBlanco
resuelveJuegoAux entorno (orden:ordenes) = do
    muestra (getTablero (paso entorno orden))
    let finalizado = (getFinalizado (paso entorno orden))
    if finalizado then
        do
            let reward = (getRecompensa (paso entorno orden))
            if (reward>0) then do
                print "campeón has ganao ;)"
                else do
                resuelveJuegoAux entorno []
                print "Perdedor te caiste"
        else do
        print finalizado
        resuelveJuegoAux (getTablero (paso entorno orden)) ordenes

lineaBlanco :: IO ()
lineaBlanco = do
    putChar '\n'
            