
{- estructuras básicas:
Tablero: Es el elemento principal del lenguaje. Tiene una cantidad de filas y de columnas que representan una matriz de celdas.

Cabezal: Este siempre se encuentra en una celda del tablero, a la que llamaremos celda actual: 
    Puede moverse de a una celda hacia una dirección (norte, sur, este u oeste).
    Donde se encuentre puede realizar solo dos acciones:
         - Poner una bolita de un color.
         - Sacar una bolita de un color.

Las bolitas pueden ser de alguno de estos colores: rojo, azul, verde o negro. -}
--PUNTO 1
type Tablero = [Celda] 

type Posicion = (Int, Int)

type Celda = (Posicion, [Bolita]) 

type Cabezal = (Int, Int)

data Direccion = Norte | Sur | Este | Oeste deriving (Show) -- esto es para el cabezal

data Bolita = Bolita {
    cantidad :: Int,
    color :: String
} deriving (Show)

--PUNTO 2

inicializarTablero :: Int -> Int -> Tablero
inicializarTablero fila columna = armarTablero [1..fila] [1..columna]

cabezal = (1, 1) -- inicia con el programa? que pasa si creo otro tablero?

armarTablero :: [Int] -> [Int] -> Tablero 
armarTablero xs ys = [((x, y), []) | x <- xs, y <- ys] 

--productoCartesiano :: [a] -> [b] -> [(a, b)]
--productoCartesiano xs ys = [(x,y) | x <- xs, y <- ys]

--tableroPruebas = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
{-

type Filas = Int
type Columnas = Int

type Dimension = (Filas, Columnas)

(4,4)
[1..colum][1..fila]

hacemos otra funcion

doble lista, matriz, la posicion queda implicita
esta seria una matriz de 2x3
[
  [(1, 1), (2, 1), (3, 1)],
  [(1, 2), (2, 2), (3, 2)]|
] 

 [(1, 1), (2, 1), (3, 1), (1, 2), ...]  la posicion no queda implicita, una lista

 -}
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
sacarBolita :: Bolita -> Tablero -> Tablero 


-} 
