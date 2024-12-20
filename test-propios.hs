import Data.List
import Solucion
import Test.HUnit

runAllTests = runTestTT allTests

allTests =
  test
    [ "vuelosValidos" ~: testVuelosValidos,
      "ciudadesConectadas" ~: testCiudadesConectadas,
      "modernizarFlota" ~: testModernizarFlota,
      "ciudadMasConectada" ~: testCiudadMasConectada,
      "sePuedeLlegar" ~: testSePuedeLLegar,
      "duracionDelCaminoMasRapido" ~: testDuracionDelCaminoMasRapido,
      "puedoVolverAOrigen" ~: testPuedoVolverAOrigen
    ]

origen :: String
origen = "BsAs"

destino :: String
destino = "Tucuman"

testVuelosValidos =
  test
    [ "vuelos válido sin vuelos" ~: vuelosValidos [] ~?= True,
      "vuelos válido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True,
      "vuelos válido con un elemento y duracion 0.0" ~: vuelosValidos [("BsAs", "Rosario", 0.0)] ~?= False,
      "vuelos válido con 2 vuelos diferentes"
        ~: vuelosValidos [("BsAs", "Rosario", 3.0), ("BsAs", "Tucuman", 3.0)]
        ~?= True,
      "vuelos válido con 2 vuelos iguales"
        ~: vuelosValidos [("BsAs", "Rosario", 3.0), ("BsAs", "Rosario", 3.0)]
        ~?= False,
      "vuelos válido con 5 vuelos diferentes"
        ~: vuelosValidos
          [ ("BsAs", "Rosario", 3.0),
            ("BsAs", "Tucuman", 3.0),
            ("Santa Cruz", "Camerun", 8.5),
            ("Rio Negro", "Suiza", 4.4),
            ("CABA", "La Rioja", 2.1)
          ]
        ~?= True,
      "vuelos válido con 3 vuelos diferentes con duracion negativa"
        ~: vuelosValidos
          [ ("BsAs", "Rosario", 3.0),
            ("BsAs", "Cancun", 2.4),
            ("Santa Cruz", "Camerun", -35.4)
          ]
        ~?= False,
      "vuelos válido con 3 vuelos diferentes y dos con duracion cero y uno duracion negativa"
        ~: vuelosValidos
          [ ("BsAs", "Rosario", 0.0),
            ("BsAs", "Cancun", 0.0),
            ("Santa Cruz", "Camerun", -35.4)
          ]
        ~?= False
    ]

testCiudadesConectadas =
  test
    [ "ciudades conectadas sin vuelos" ~: ciudadesConectadas [] origen ~?= [],
      expectPermutacion (ciudadesConectadas [(origen, "Rosario", 5.0)] origen) ["Rosario"],
      expectPermutacion (ciudadesConectadas [(origen, "Rosario", 3.0), (origen, "Tucuman", 3.0)] origen) ["Rosario", "Tucuman"],
      expectPermutacion (ciudadesConectadas [(origen, "Rosario", 3.0), ("Tucuman", origen, 3.0), ("La Quiaca", origen, 3.0) ] origen) ["Rosario", "Tucuman", "La Quiaca"],
      expectPermutacion (ciudadesConectadas [(origen, "Rosario", 3.0), ("Tucuman", origen, 3.0), ("La Quiaca", origen, 3.0) ] "Brasilia") [],
      expectPermutacion (ciudadesConectadas [(origen, "Rosario", 3.0), ("Tucuman", origen, 2.0), ("La Quiaca", origen, 8.4), (origen, "Tucuman", 2.45) ] origen) ["Rosario", "Tucuman", "La Quiaca"],
      expectPermutacion (ciudadesConectadas [(origen, "Rosario", 3.0), ("Tucuman", "Salta", 3.0), ("La Quiaca", origen, 3.0) ] origen) ["Rosario", "La Quiaca"] 
    ]

testModernizarFlota =
  test
    [ "modernizar flota sin vuelos" ~: modernizarFlota [] ~?= [],
      expectPermutacion (modernizarFlota [("BsAs", "Rosario", 10.0)]) [("BsAs", "Rosario", 9.0)],
      expectPermutacion (modernizarFlota [("BsAs", "Rosario", 57.0), ("La Plata", "Ciudad Chistosa", 20.0)]) [("BsAs", "Rosario", 51.3), ("La Plata", "Ciudad Chistosa", 18.0)],
      expectPermutacion (modernizarFlota [ ("BsAs", "Rosario", 57.0), ("La Plata", "Ciudad Chistosa", 20.0), ("La Pampa", "La Rioja", 4.0) ]) [("BsAs", "Rosario", 51.3), ("La Plata", "Ciudad Chistosa", 18.0), ("La Pampa", "La Rioja", 3.6)]
    ]

testCiudadMasConectada =
  test
    [ "ciudad mas conectada sin vuelos" ~: ciudadMasConectada [] ~?= [],
      "ciudad mas conectada con un vuelo" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0)] ~?= "Rosario",
      "ciudad mas conectada con dos vuelos"
        ~: ciudadMasConectada [("BsAs", "Rosario", 57.0), ("BsAs", "Ciudad Chistosa", 20.0)]
        ~?= "BsAs",
      "ciudad mas conectada con tres vuelos"
        ~: ciudadMasConectada
          [ ("BsAs", "Rosario", 57.0),
            ("La Plata", "Ciudad Chistosa", 20.0),
            ("La Pampa", "La Plata", 4.5)
          ]
        ~?= "La Plata",
      "ciudad mas conectada con dos posibles resultados"
        ~: expectAny 
          (ciudadMasConectada [("BsAs", "Rosario", 57.0), ("La Plata", "Ciudad Chistosa", 20.0), ("La Pampa", "La Plata", 4.5), ("Ciudad Chistosa", "Ciudad Aburrida", 4.5)]) 
          (["La Plata", "Ciudad Chistosa"])
    ]

testSePuedeLLegar =
  test
    [ "no se puede llegar sin vuelos" ~: sePuedeLlegar [] origen destino ~?= False,
      "se puede llegar con un vuelo que matchea"
        ~: sePuedeLlegar [(origen, destino, 10.0)] origen destino
        ~?= True,
      "no se puede llegar con un vuelo que no matchea"
        ~: sePuedeLlegar [(origen, "Rosario", 10.0)] origen destino
        ~?= False,
      "se puede llegar sin escala con lista con multiples vuelos"
        ~: sePuedeLlegar
          [ (origen, "Rosario", 10.0),
            ("Cordoba", destino, 10.0),
            (origen, destino, 10.0),
            ("Rosario", destino, 10.0)
          ]
          origen
          destino
        ~?= True,
      "se puede llegar con maximo una escala con multiples vuelos, encontrando primero el origen"
        ~: sePuedeLlegar
          [ (origen, "Rosario", 10.0),
            ("Cordoba", destino, 10.0),
            (origen, "La Plata", 10.0),
            ("Konoha", "Ciudad Chistosa", 10.0),
            ("Rosario", "Brasil", 10.0),
            ("Rosario", destino, 10.0),
            ("Rosario", "Bulnes", 10.0)
          ]
          origen
          destino
        ~?= True,
      "se puede llegar con maximo una escala con multiples vuelos, encontrando primero el destino"
        ~: sePuedeLlegar
          [ ("Rosario", destino, 10.0),
            ("Cordoba", destino, 10.0),
            (origen, "La Plata", 10.0),
            ("Konoha", "Ciudad Chistosa", 10.0),
            ("Rosario", "Brasil", 10.0),
            (origen, "Rosario", 10.0),
            ("Rosario", "Bulnes", 10.0)
          ]
          origen
          destino
        ~?= True
    ]

testDuracionDelCaminoMasRapido =
  test
    [ "duracion del camino mas rapido con un vuelo"
        ~: aproximado (duracionDelCaminoMasRapido [(origen, destino, 10.0)] origen destino) 10.0
        ~?= True,
      "duracion del camino mas rapido con una escala"
        ~: aproximado (duracionDelCaminoMasRapido
          [ (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3)
          ]
          origen
          destino) 7.3
        ~?= True,
      "duracion del camino mas rapido con un vuelo directo y uno con escala, donde directo es mas rapido"
        ~: aproximado (duracionDelCaminoMasRapido
          [ (origen, destino, 3.4),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3)
          ]
          origen
          destino) 3.4
        ~?= True,
      "duracion del camino mas rapido con un vuelo directo y uno con escala, donde escala es mas rapido"
        ~: aproximado (duracionDelCaminoMasRapido
          [ (origen, destino, 9.8),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3)
          ]
          origen
          destino) 7.3
        ~?= True,
      "duracion del camino mas rapido con un vuelo directo y dos con escala, donde directo es mas rapido"
        ~: aproximado (duracionDelCaminoMasRapido
          [ (origen, destino, 4.8),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3),
            (origen, "La Plata", 2.0),
            ("La Plata", destino, 3.3)
          ]
          origen
          destino) 4.8
        ~?= True,
      "duracion del camino mas rapido con un vuelo directo y dos con escalas, donde una escala es mas rapida"
        ~: aproximado (duracionDelCaminoMasRapido
          [ (origen, destino, 9.8),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3),
            (origen, "La Plata", 2.0),
            ("La Plata", destino, 3.3)
          ]
          origen
          destino) 5.3
        ~?= True
    ]

testPuedoVolverAOrigen =
  test
    [ "no puedo volver al origen sin vuelos"
        ~: puedoVolverAOrigen [] origen
        ~?= False,
      "no puedo volver al origen con un vuelo"
        ~: puedoVolverAOrigen [(origen, "La Plata", 5.5)] origen
        ~?= False,
      "puedo volver al origen con 2 vuelos, ida y vuelta directa"
        ~: puedoVolverAOrigen [(origen, "La Plata", 5.5), ("La Plata", origen, 5.5)] origen
        ~?= True,
      "puedo volver al origen con 3 vuelos, ida y vuelta con 1 escala"
        ~: puedoVolverAOrigen
          [ (origen, "La Plata", 5.5),
            ("La Plata", "San Juan", 5.5),
            ("San Juan", origen, 5.5)
          ]
          origen
        ~?= True,
      "puedo volver al origen con muchos vuelos, ida y vuelta con 1 escala y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          [ ("Rosario", "La Plata", 5.5),
            ("Cordoba", "La Rioja", 5.5),
            (origen, "La Rioja", 5.5), -- Salida 1
            ("Neuquen", "Misiones", 5.5),
            ("Rio Negro", "La Quiaca", 5.5),
            ("La Rioja", "Santa Fe", 5.5), -- Primera Escala 1
            (origen, "La Plata", 5.5), -- Salida 2
            ("La Plata", "San Juan", 5.5), -- Primera Escala 2
            ("Santa Fe", "Chubut", 5.5), -- Segunda Escala 1 -> Dead End
            ("San Juan", origen, 5.5) -- Llegada 2
          ]
          origen
        ~?= True,
      "no puedo volver al origen con dos posibilidades, muchos vuelos y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          [ ("Rosario", "La Plata", 5.5),
            ("Cordoba", "La Rioja", 5.5),
            (origen, "La Rioja", 5.5), -- Salida 1
            ("Neuquen", "Misiones", 5.5),
            ("Rio Negro", "La Quiaca", 5.5),
            ("La Rioja", "Santa Fe", 5.5), -- Primera Escala 1
            (origen, "La Plata", 5.5), -- Salida 2
            ("La Plata", "San Juan", 5.5), -- Primera Escala 2
            ("Santa Fe", "Chubut", 5.5), -- Segunda Escala 1 -> Dead End 1
            ("San Juan", "Rusia", 5.5) -- Segunda Escala 2 -> Dead End 2
          ]
          origen
        ~?= False,
      "puedo volver al origen con muchos vuelos, ida y vuelta con 2 escalas y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          -- Creo que los comentarios mas que sumar restan en este porque hay muchos dead ends
          -- No agregue todos, deje algunos
          [ ("Rosario", "La Plata", 5.5),
            ("Cordoba", "La Rioja", 5.5),
            ("La Plata", "La Rioja", 5.5), -- Primera Escala 2a
            ("Jujuy", origen, 5.5), -- Llegada 2b
            ("Santa Fe", "Ezeiza", 5.5),
            (origen, "La Rioja", 5.5), -- Salida 1
            ("Neuquen", "Misiones", 5.5),
            ("La Rioja", "Trenquelaunquen", 5.5), -- Primera Escala 1a y Segunda Escala 2a
            ("Rio Negro", "La Quiaca", 5.5),
            ("La Rioja", "Santa Fe", 5.5), -- Primera Escala 1b
            (origen, "La Plata", 5.5), -- Salida 2
            ("La Plata", "San Juan", 5.5), -- Primera Escala 2b
            ("Santa Fe", "Chubut", 5.5), -- Segunda Escala 1b y Tercera Escala 2a -> Dead End
            ("Trenquelaunquen", "Rusia", 5.5), -- Segunda Escala 1a -> Dead End
            ("San Juan", "Jujuy", 5.5), -- Segunda Escala 2b
            ("Ezeiza", "España", 5.5)
          ]
          origen
        ~?= True,
      "no puedo volver al origen con muchos vuelos, ida y vuelta con 2 escalas y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          [ ("Rosario", "La Plata", 5.5),
            ("Cordoba", "La Rioja", 5.5),
            ("La Plata", "La Rioja", 5.5),
            ("Jujuy", "Salta", 5.5),
            ("Santa Fe", "Ezeiza", 5.5),
            (origen, "La Rioja", 5.5),
            ("Neuquen", "Misiones", 5.5),
            ("La Rioja", "Trenquelaunquen", 5.5),
            ("Rio Negro", "La Quiaca", 5.5),
            ("La Rioja", "Santa Fe", 5.5),
            (origen, "La Plata", 5.5),
            ("La Plata", "San Juan", 5.5),
            ("Santa Fe", "Chubut", 5.5),
            ("Trenquelaunquen", "Rusia", 5.5),
            ("San Juan", "Jujuy", 5.5),
            ("Ezeiza", "España", 5.5)
          ]
          origen
        ~?= False
    ]


-- Auxiliares
-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: (Ord a) => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
