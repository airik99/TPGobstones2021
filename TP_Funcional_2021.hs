import Text.Show.Functions
import Data.List

--------- PUNTO 1 ---------

type Tablero = ([Celda], Cabezal)

type Posicion = (Int, Int)

type Celda = (Posicion, Bolitas) 

type Cabezal = (Int, Int)

data Direccion = Norte | Sur | Este | Oeste deriving (Show) 

data Bolita = Azul | Negro | Rojo | Verde deriving (Show) 

data Bolitas = Bolitas {
    azul :: Int,
    negro :: Int,
    rojo :: Int,
    verde :: Int
} deriving (Show, Eq)

type Sentencia = Tablero -> Tablero

type Condicion = Tablero -> Bool

modificarAzules :: (Int -> Int) -> Bolitas -> Bolitas
modificarAzules funcion unaBolita = unaBolita {azul = funcion . azul $ unaBolita}

modificarNegros :: (Int -> Int) -> Bolitas -> Bolitas
modificarNegros funcion unaBolita = unaBolita {negro = funcion . negro $ unaBolita}   

modificarRojos :: (Int -> Int) -> Bolitas -> Bolitas
modificarRojos funcion unaBolita = unaBolita {rojo = funcion . rojo $ unaBolita}   

modificarVerdes :: (Int -> Int) -> Bolitas -> Bolitas
modificarVerdes funcion unaBolita = unaBolita {verde = funcion . verde $ unaBolita}   
 
--------- PUNTO 2 ---------

inicializarTablero :: Int -> Int -> Tablero
inicializarTablero fila columna = (armarTablero [1..fila] [1..columna], (1, 1))

armarTablero :: [Int] -> [Int] -> [Celda]
armarTablero xs ys = [((x, y), Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}) | x <- xs, y <- ys] 

--------- PUNTO 3 ---------
--------- MOVER ---------

mover :: Direccion -> Sentencia
mover direccion tablero | puedeMoverse direccion tablero = alterarCabezal (actualizacionDePosicion direccion) tablero
                        | otherwise = error "El cabezal se cayo del tablero"

actualizacionDePosicion :: Direccion -> Cabezal -> Cabezal 
actualizacionDePosicion Sur (x, y) = (x - 1, y)

actualizacionDePosicion Norte (x, y) = (x + 1, y)

actualizacionDePosicion Este (x, y) = (x, y + 1)

actualizacionDePosicion Oeste (x, y) = (x, y - 1)

esPosicionValida :: Cabezal -> [Celda] -> Bool                            
esPosicionValida cabezal = (elem cabezal) . conseguirPosiciones

conseguirPosiciones :: [Celda] -> [Posicion]
conseguirPosiciones celdas = map fst celdas

alterarCabezal :: (Cabezal -> Cabezal) -> Tablero -> Tablero
alterarCabezal funcionModificadora (celdas, cabezal) = (celdas, funcionModificadora cabezal)

--------- PONER --------- 

poner :: Bolita -> Sentencia
poner color tablero = modificarCeldaActual (agregarBolita color) tablero

modificarCeldaActual :: (Celda -> Celda) -> Tablero -> Tablero 
modificarCeldaActual funcion (celdas, cabezal) = (map (enCeldaEspecifica cabezal funcion) celdas, cabezal) 

enCeldaEspecifica :: Cabezal -> (Celda -> Celda) -> Celda -> Celda
enCeldaEspecifica cabezal funcion celda | cabezal == (fst celda) = funcion celda
                                        | otherwise              = celda

agregarBolita :: Bolita -> Celda -> Celda
agregarBolita color = sumarleUna (alterarEseColor color)

sumarleUna :: ((Int -> Int) -> Bolitas -> Bolitas) -> Celda -> Celda
sumarleUna cambiarCantidadDeEseColor = alterarCantidadBolitas (cambiarCantidadDeEseColor (+ 1))

alterarEseColor :: Bolita -> (Int -> Int) -> Bolitas -> Bolitas
alterarEseColor Azul = modificarAzules
alterarEseColor Negro = modificarNegros
alterarEseColor Rojo = modificarRojos
alterarEseColor Verde = modificarVerdes

alterarCantidadBolitas :: (Bolitas -> Bolitas) -> Celda -> Celda
alterarCantidadBolitas funcionModificadora (posicion, bolitas) = (posicion, funcionModificadora bolitas) 

--------- SACAR --------- Dio bien el punto 7!!!!!!!!!

sacar :: Bolita -> Sentencia
sacar color = modificarCeldaActual (sacarBolita color)

sacarBolita :: Bolita -> Celda -> Celda
sacarBolita color celda | hayBolitasEnCelda color celda = restarleUna (alterarEseColor color) celda 
                        | otherwise = error ("No hay bolitas " ++ show color ++ " para sacar de la celda actual")

restarleUna :: ((Int -> Int) -> Bolitas -> Bolitas) -> Celda -> Celda
restarleUna cambiarCantidadDeEseColor = alterarCantidadBolitas (cambiarCantidadDeEseColor (subtract 1))

hayBolitasEnCelda :: Bolita -> Celda -> Bool
hayBolitasEnCelda color = (> 0) . cantidadDeBolitasEnCelda color

cantidadDeBolitasEnCelda :: Bolita -> Celda -> Int
cantidadDeBolitasEnCelda Rojo = rojo . obtenerBolitas
cantidadDeBolitasEnCelda Verde = verde . obtenerBolitas
cantidadDeBolitasEnCelda Azul = azul . obtenerBolitas
cantidadDeBolitasEnCelda Negro = negro . obtenerBolitas

obtenerBolitas :: Celda -> Bolitas 
obtenerBolitas = snd

--------- PUNTO 4 ---------
--------- REPETIR ---------

repetir :: Int -> [Sentencia] -> Tablero -> Tablero
repetir cantidad sentencias tablero = (!! cantidad) . (iterate (programa sentencias)) $ tablero

--------- ALTERNATIVA ---------

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Tablero -> Tablero
alternativa condicion sentencias1 sentencias2 tablero | condicion tablero = programa sentencias1 tablero
                                                      | otherwise         = programa sentencias2 tablero

--------- SI --------

si :: Condicion -> [Sentencia] -> Tablero -> Tablero
si condicion sentencias = alternativa condicion sentencias []

--------- SINO ---------

sino :: Condicion -> [Sentencia] -> Tablero -> Tablero
sino condicion sentencias = alternativa (not . condicion) sentencias []

--------- MIENTRAS ---------

mientras :: Condicion -> [Sentencia] -> Tablero -> Tablero
mientras condicion sentencias = si condicion (sentencias ++ [mientras condicion sentencias])

--------- IR AL BORDE ---------

irAlBorde :: Direccion -> Tablero -> Tablero
irAlBorde direccion tablero = mientras (puedeMoverse direccion) [mover direccion] tablero

--------- PUNTO 5 ---------
--------- PUEDE MOVERSE ---------

puedeMoverse :: Direccion -> Tablero -> Bool
puedeMoverse direccion (celdas, cabezal) = esPosicionValida (actualizacionDePosicion direccion cabezal) celdas

--------- HAY BOLITA ---------

hayBolitas :: Bolita -> Tablero -> Bool
hayBolitas color = (> 0) . (cantidadDeBolitas color)

--------- CANTIDAD DE BOLITAS ---------

cantidadDeBolitas :: Bolita -> Tablero -> Int
cantidadDeBolitas Rojo = rojo . snd . obtenerCeldaActual
cantidadDeBolitas Verde = verde . snd . obtenerCeldaActual
cantidadDeBolitas Azul = azul . snd . obtenerCeldaActual
cantidadDeBolitas Negro = negro . snd . obtenerCeldaActual

obtenerCeldaActual :: Tablero -> Celda
obtenerCeldaActual ([celda], cabezal) = celda
obtenerCeldaActual ((celdaCabeza:celdaCola), cabezal) | fst celdaCabeza == cabezal = celdaCabeza
                                                      | otherwise                  = obtenerCeldaActual (celdaCola, cabezal)

--------- PUNTO 6 ---------

programa :: [Sentencia] -> Tablero -> Tablero
programa sentencias tablero = foldl (flip ($)) tablero sentencias

--------- PUNTO 7 --------

hacerPunto7 = programa sentenciasPunto7 tablero

sentenciasPunto7 :: [Sentencia]  
sentenciasPunto7 = [mover Norte, 
                    poner Negro, 
                    poner Negro, 
                    poner Azul, 
                    mover Norte, 
                    repetir 15 
                        [poner Rojo, poner Azul], 
                    alternativa (hayBolitas Verde) 
                        [mover Este, poner Negro] 
                        [mover Sur, mover Este, poner Azul], 
                    mover Este, 
                    mientras ((<= 9) . (cantidadDeBolitas Verde)) 
                        [poner Verde, poner Azul]
                    ]

--------------------------- PRUEBAS ---------------------------

tablero :: Tablero
tablero = ([((1,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((1,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((1,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0})],(1,1))

tableroPruebas :: Tablero
tableroPruebas = ([((1,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((1,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((1,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,1),Bolitas {azul = 2, negro = 1, rojo = 0, verde = 4}),((3,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0})],(1,1))

celdasPruebas :: [Celda]
celdasPruebas = [((1,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((1,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((1,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((2,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,1),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,2),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0}),((3,3),Bolitas {azul = 0, negro = 0, rojo = 0, verde = 0})]

bolitas :: Bolitas
bolitas = Bolitas {azul = 1, negro = 2, rojo = 0, verde = 4}

condicionFalsa :: Condicion 
condicionFalsa (celdas, cabezal) = cabezal == (1, 3) 

condicionVerdadera :: Condicion
condicionVerdadera (celdas, cabezal) = cabezal == (1, 1)