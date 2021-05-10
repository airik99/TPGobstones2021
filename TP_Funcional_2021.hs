
{- estructuras básicas:
Tablero: Es el elemento principal del lenguaje. Tiene una cantidad de filas y de columnas que representan una matriz de celdas.

Cabezal: Este siempre se encuentra en una celda del tablero, a la que llamaremos celda actual: 
    Puede moverse de a una celda hacia una dirección (norte, sur, este u oeste).
    Donde se encuentre puede realizar solo dos acciones:
         - Poner una bolita de un color.
         - Sacar una bolita de un color.

Las bolitas pueden ser de alguno de estos colores: rojo, azul, verde o negro. -}
type Tablero = [Celda]

type Posicion = (Int, Int)

type Celda = ((Int, Bolita), (Int, Bolita), (Int, Bolita), (Int, Bolita), Posicion)

type Cabezal = (Int, Int)

data Direccion = Norte | Sur | Este | Oeste deriving (Show)

data Bolita = Rojo | Negro | Azul | Verde deriving (Show)

-- celda = [(1, rojo)] ++ [(1, azul)]
--x:y:xs
--         Ancho   Alto
{-crearTablero :: Int -> Int -> Tablero --cabezal en (1,1) //punto 2
crearTablero fila columna = 
                                        | a  b |
--usar replicate                        | c  d |
-- nuestro tablero se veria (2x2)
--  [((1, Azul), (0, Negro), (0, Rojo), (0, Verde), (1, 1)), ((2, Azul), (3, Negro), (0, Rojo), (0, Verde), (1, 2))]
-- [((7, Azul), (1, Negro), (0, Rojo), (0, Verde), (2, 1)), ((6, Azul), (3, Negro), (1, Rojo), (0, Verde), (2, 2))]

--

moverCabezal :: Direccion -> Tablero -> Tablero

--
agregarBolita :: Bolita -> Tablero -> Tablero

-- 
sacarBolita :: Bolita -> Tablero -> Tablero -} 