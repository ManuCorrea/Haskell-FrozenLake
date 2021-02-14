-- Trabajo Final Programación Declarativa.

-- Desarrollado por los alumnos:
-- Manuel Correa Gomez (mancorgom)
-- Alejandro Jiménez Martín (alejimmar)

-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

import I1M.Pila
--import PilaConTipoDeDatoAlgebraico
import Data.Matrix as M

import FrozenLake
import Algoritmo


{- 
Función principal que hemos creado para resolver el juego mostrando el camino realizado por el algoritmo.
 
Le pasamos como parámetros:
    * entorno -> Entorno válido generado aleatoriamente.
 
El resultado de la función es una secuencia de tableros con todos los movimimentos desde la salida 
hasta la meta, escribiendo en cada caso la posición en la que estamos con una 'X'.
 
Para probar el método:
resuelveJuego (iniciaEntorno 5 123)
-}
resuelveJuego :: Entorno -> IO ()
resuelveJuego entorno = do 
    lineaBlanco
    print "Empieza el juego:"
    muestra entorno
    lineaBlanco
    print "Siguiente movimiento:"
    resuelveJuegoAux entorno (rutaAOrdenes (obtenerCaminoValido (listaDFS ++ meta)) [])
    where
        listaDFS = trazaDFS (fst entorno) (snd entorno) -- obtiene las soluciones
        meta = [(tamano, tamano)]
        tamano = nrows (fst entorno) 


resuelveJuegoAux :: Entorno -> [Accion] -> IO ()
resuelveJuegoAux entorno [] = lineaBlanco
resuelveJuegoAux entorno (orden:ordenes) = do
    muestra (getTablero (paso entorno orden))
    let finalizado = (getFinalizado (paso entorno orden))
    if finalizado then
        do
            lineaBlanco
            print "El juego ha terminado"
            lineaBlanco
            let reward = (getRecompensa (paso entorno orden))
            if (reward>0) then do
                print "Has ganado ;)"
                else do
                resuelveJuegoAux entorno []
                print "Perdiste :("
        else do
            lineaBlanco
            print "Siguiente movimiento:"
            resuelveJuegoAux (getTablero (paso entorno orden)) ordenes



{- 
Función que nos pinta una linea en blanco.
-}
lineaBlanco :: IO ()
lineaBlanco = do
    putChar '\n'
            