data Tablero = Tab Posiciones Posiciones
            deriving Show

type Posicion = (Int,Int)

type Posiciones = [Posicion]

tableroInicial :: Tablero
tableroInicial = Tab [] []