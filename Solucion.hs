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

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos [vuelo] = vueloValido vuelo
vuelosValidos (vuelo : resto) =
  vueloValido vuelo
    && not (vueloEsRepetido vuelo resto)
    && vuelosValidos resto

-- a [b, a]
vueloEsRepetido :: Vuelo -> AgenciaDeViajes -> Bool
vueloEsRepetido vuelo [] = False
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
ciudadesConectadas _ _ = ["BsAs"] -- Borrar y escribir el código correcto

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota _ = [("BsAs", "Rosario", 9.0)] -- Borrar y escribir el código correcto

-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada _ = "Rosario" -- Borrar y escribir el código correcto

-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino = True -- Borrar y escribir el código correcto

-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido _ _ _ = 10.0 -- Borrar y escribir el código correcto

-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto
