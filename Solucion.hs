{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}
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
vuelosValidos [vuelo] = vueloValido vuelo
vuelosValidos (vuelo : resto) =
  vueloValido vuelo
    && not (vueloEsRepetido vuelo resto)
    && vuelosValidos resto

vueloEsRepetido :: Vuelo -> AgenciaDeViajes -> Bool
vueloEsRepetido _ [] = False
vueloEsRepetido vuelo [ultimo] = compararVuelos vuelo ultimo
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
ciudadesConectadas vuelos ciudad = eliminarDuplicados (resolver vuelos ciudad)
  where
    resolver :: AgenciaDeViajes -> Ciudad -> [Ciudad]
    resolver [] _ = []
    resolver ((origen, destino, _) : resto) ciudad
      | ciudad == origen = destino : resolver resto ciudad
      | ciudad == destino = origen : resolver resto ciudad
      | otherwise = resolver resto ciudad

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((origen, destino, duracion) : resto) =
  (origen, destino, duracion * 0.9) : modernizarFlota resto

-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada [] = []
ciudadMasConectada vuelos = masRepetido (resolver vuelos)
  where
    resolver :: AgenciaDeViajes -> [Ciudad]
    resolver [] = []
    resolver ((origen, destino, _) : resto) = origen : destino : resolver resto

-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False
sePuedeLlegar vuelos origen destino =
  buscarSinEscala vuelos origen destino /= vueloNulo
    || length (buscarConEscala vuelos origen destino) > 0

buscarSinEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> Vuelo
buscarSinEscala [] _ _ = ("", "", 0)
buscarSinEscala (vuelo : resto) origen destino
  | origen == principio && final == destino = vuelo
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
  | destino == final -- Jeje
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
    duraciones = sinEscala : duracionesConEscala conEscala
    (_, _, sinEscala) = buscarSinEscala vuelos origen destino
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
puedoVolverAOrigen vuelos origen =
  buscarConMultiplesEscalas vuelos origen || puedoVolverAOrigen (tail vuelos) origen

buscarIdaYVueltaSinEscala :: AgenciaDeViajes -> Ciudad -> Bool
buscarIdaYVueltaSinEscala [] _ = False
buscarIdaYVueltaSinEscala ((principio, final, _) : resto) origen
  | principio == origen = buscarSinEscala resto final origen /= vueloNulo
  | otherwise = buscarIdaYVueltaSinEscala resto origen

-- buscarIdaYVueltaConEscala :: AgenciaDeViajes -> Ciudad -> Bool
-- buscarIdaYVueltaConEscala [] _ = False
-- buscarIdaYVueltaConEscala ((principio, final, _) : resto) origen
--   | principio == origen = buscarConEscala resto final origen /= vueloNulo
--   | otherwise = buscarIdaYVueltaConEscala resto origen

buscarConMultiplesEscalas :: AgenciaDeViajes -> Ciudad -> Bool
buscarConMultiplesEscalas ((principio, final, _) : resto) origen
  | principio == origen = sinEscala /= vueloNulo || conEscala
  | otherwise = buscarConMultiplesEscalas resto origen
  where
    sinEscala = buscarSinEscala resto final origen
    conEscala = buscarIdaYVueltaConMultiplesEscalas resto origen

buscarIdaYVueltaConMultiplesEscalas :: AgenciaDeViajes -> Ciudad -> Bool
buscarIdaYVueltaConMultiplesEscalas ((principio, final, _) : resto) origen = resolver
  where
    resolver :: Bool
    resolver = buscarSinEscala resto final origen /= vueloNulo

-- Auxiliares

eliminarDuplicados :: (Eq t) => [t] -> [t]
eliminarDuplicados [] = []
eliminarDuplicados (x : xs) = x : eliminarDuplicados (eliminarOcurrencias x xs)

eliminarOcurrencias :: (Eq t) => t -> [t] -> [t]
eliminarOcurrencias a [] = []
eliminarOcurrencias a (b : rest)
  | a == b = eliminarOcurrencias a rest
  | otherwise = b : eliminarOcurrencias a rest

masRepetido :: (Eq t) => [t] -> t
masRepetido [a] = a
masRepetido lista = resolver lista lista
  where
    resolver :: (Eq t) => [t] -> [t] -> t
    resolver lista [a, b]
      | cantidadApariciones a lista > cantidadApariciones b lista = a
      | otherwise = b
    resolver lista (a : b : rest)
      | cantidadApariciones a lista > cantidadApariciones b lista =
          resolver lista (a : rest)
      | otherwise = resolver lista (b : rest)

cantidadApariciones :: (Eq t) => t -> [t] -> Int
cantidadApariciones _ [] = 0
cantidadApariciones a (b : rest)
  | a == b = 1 + cantidadApariciones a rest
  | otherwise = cantidadApariciones a rest
