module Solucion where

-- Completar!
-- Nombre de grupo: {null_was_a_mistake}
-- Integrante1: { 95219236, Gustavo Alexis Viana Lucas}
-- Integrante2: { 45630844, Gonzalo Tomas Quiza}
-- Integrante3: { 43630634, Leandro Sebastián Díaz Rojas}

type Ciudad = String

type Duracion = Float

type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

vueloNulo :: Vuelo
vueloNulo = ("", "", 0.0)

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (vuelo : resto) =
  vueloValido vuelo
    && not (vueloEsRepetido vuelo resto)
    && vuelosValidos resto

vueloEsRepetido :: Vuelo -> AgenciaDeViajes -> Bool
vueloEsRepetido _ [] = False
vueloEsRepetido vuelo (primero : resto) =
  compararVuelos vuelo primero || vueloEsRepetido vuelo resto

compararVuelos :: Vuelo -> Vuelo -> Bool
compararVuelos (origen1, destino1, _) (origen2, destino2, _) =
  origen1 == origen2 && destino1 == destino2

vueloValido :: Vuelo -> Bool
vueloValido (origen, destino, duracion) = duracion > 0 && origen /= destino

-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = []
ciudadesConectadas vuelos ciudad = eliminarDuplicados (obtenerCiudadesConectadas vuelos)
  where
    obtenerCiudadesConectadas :: AgenciaDeViajes -> [Ciudad]
    obtenerCiudadesConectadas [] = []
    obtenerCiudadesConectadas ((origen, destino, _) : resto)
      | ciudad == origen = destino : obtenerCiudadesConectadas resto
      | ciudad == destino = origen : obtenerCiudadesConectadas resto
      | otherwise = obtenerCiudadesConectadas resto

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((origen, destino, duracion) : resto) =
  (origen, destino, duracion * 0.9) : modernizarFlota resto

-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada [] = []
ciudadMasConectada vuelos = masRepetido (desestructurar vuelos)
  where
    desestructurar :: AgenciaDeViajes -> [Ciudad]
    desestructurar [] = []
    desestructurar ((origen, destino, _) : resto) =
      origen : destino : desestructurar resto

-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False
sePuedeLlegar vuelos origen destino =
  buscarSinEscala vuelos origen destino /= vueloNulo
    || length (buscarConEscala vuelos origen destino) > 0

buscarSinEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> Vuelo
buscarSinEscala [] _ _ = vueloNulo
buscarSinEscala (vuelo : resto) origen destino
  | principio == origen && destino == final = vuelo
  | otherwise = buscarSinEscala resto origen destino
  where
    (principio, final, duracion) = vuelo

buscarConEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
buscarConEscala [] _ _ = []
buscarConEscala (vuelo : resto) origen destino
  | origen == principio && vueloDestinoFinalSinEscala /= vueloNulo =
      vuelo
        : vueloDestinoFinalSinEscala
        : buscarConEscala resto origen destino
  | destino == final -- jeje
      && vueloOrigenPrincipioSinEscala /= vueloNulo =
      vuelo : vueloOrigenPrincipioSinEscala : buscarConEscala resto origen destino
  | otherwise = buscarConEscala resto origen destino
  where
    (principio, final, duracion) = vuelo
    vueloDestinoFinalSinEscala = buscarSinEscala resto final destino -- vuelo 180 go boom
    vueloOrigenPrincipioSinEscala = buscarSinEscala resto origen principio

-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido vuelos origen destino = minimoEnDuraciones duraciones
  where
    duraciones
      | vueloSinEscala == vueloNulo = duracionesConEscala conEscala
      | otherwise = sinEscala : duracionesConEscala conEscala
    (_, _, sinEscala) = vueloSinEscala
    vueloSinEscala = buscarSinEscala vuelos origen destino
    conEscala = buscarConEscala vuelos origen destino
    duracionesConEscala :: AgenciaDeViajes -> [Duracion]
    duracionesConEscala [] = []
    duracionesConEscala ((_, _, duracion1) : (_, _, duracion2) : resto) =
      (duracion1 + duracion2) : duracionesConEscala resto

minimo :: Float -> Float -> Float
minimo primero segundo
  | primero < segundo = primero
  | otherwise = segundo

minimoEnDuraciones :: [Float] -> Float
minimoEnDuraciones [duracion] = duracion
minimoEnDuraciones (primero : segundo : resto) =
  minimo primero minimoDeSegundo
  where
    minimoDeSegundo = minimoEnDuraciones (segundo : resto)

-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen [] _ = False
puedoVolverAOrigen vuelos ciudad = busquedaDeArbol vuelos vuelos ciudad ciudad

-- Explicacion breve: usa una estructura de arbol para ir armando el vuelo de vuelta.
--
-- Por cada caso parcial que encuentra (origen es el correcto pero destino no) arma una subrama
-- eliminando el elemento actual y en esa subrama usa el destino como origen y sigue buscando
-- la ciudad original, así hasta que se queda sin vuelos. Cuando eso ocurre sigue buscando en
-- su rama y armando subramas cuando sea necesario.
--
-- Link al diagrama en Excalidraw (a la derecha de la tabla de recursiones):
-- https://excalidraw.com/#json=laMAF89IzLNy_J8pKTxDq,KTouN2yxwruCoP_fplb4Kg
--
-- En Excalidraw:
-- Apretar rueda del mouse (o icono de mano en la barra de arriba): moverse en el dibujo
-- CTRL+rueda del mouse: zoom-in/zoom-out
busquedaDeArbol :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
busquedaDeArbol [] _ _ _ = False
busquedaDeArbol _ [] _ _ = False
busquedaDeArbol ramaPrincipal (vuelo : resto) origen destino
  | origen == principio = destino == final || buscarEnSubrama || buscarEnMismaRama
  | otherwise = saltearVuelo
  where
    (principio, final, _) = vuelo
    listaFiltrada = eliminarOcurrencias vuelo ramaPrincipal
    buscarEnSubrama = busquedaDeArbol listaFiltrada listaFiltrada final destino
    buscarEnMismaRama = busquedaDeArbol listaFiltrada resto origen destino
    saltearVuelo = busquedaDeArbol ramaPrincipal resto origen destino

-- Auxiliares
eliminarDuplicados :: (Eq t) => [t] -> [t]
eliminarDuplicados [] = []
eliminarDuplicados (x : xs) = x : eliminarDuplicados (eliminarOcurrencias x xs)

eliminarOcurrencias :: (Eq t) => t -> [t] -> [t]
eliminarOcurrencias _ [] = []
eliminarOcurrencias a (b : rest)
  | a == b = eliminarOcurrencias a rest
  | otherwise = b : eliminarOcurrencias a rest

masRepetido :: (Eq t) => [t] -> t
masRepetido [a] = a
masRepetido lista = encontrarMasRepetido lista lista
  where
    encontrarMasRepetido :: (Eq t) => [t] -> [t] -> t
    encontrarMasRepetido lista [a, b]
      | cantidadApariciones a lista > cantidadApariciones b lista = a
      | otherwise = b
    encontrarMasRepetido lista (a : b : rest)
      | cantidadApariciones a lista > cantidadApariciones b lista =
          encontrarMasRepetido lista (a : rest)
      | otherwise = encontrarMasRepetido lista (b : rest)

cantidadApariciones :: (Eq t) => t -> [t] -> Int
cantidadApariciones _ [] = 0
cantidadApariciones a (b : rest)
  | a == b = 1 + cantidadApariciones a rest
  | otherwise = cantidadApariciones a rest
