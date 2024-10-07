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
        ~?= False
    ]

testCiudadesConectadas =
  test
    [ "ciudades conectadas sin vuelos" ~: ciudadesConectadas [] origen ~?= [],
      "ciudades conectadas con un elemento"
        ~: ciudadesConectadas [(origen, "Rosario", 5.0)] origen
        ~?= ["Rosario"],
      "ciudades conectadas con 2 vuelos, mismo origen, buscando ese origen"
        ~: ciudadesConectadas [(origen, "Rosario", 3.0), (origen, "Tucuman", 3.0)] origen
        ~?= ["Rosario", "Tucuman"],
      "ciudades conectadas con 3 vuelos, objetivo intercalado"
        ~: ciudadesConectadas
          [ (origen, "Rosario", 3.0),
            ("Tucuman", origen, 3.0),
            ("La Quiaca", origen, 3.0)
          ]
          origen
        ~?= ["Rosario", "Tucuman", "La Quiaca"],
      "ciudades conectadas con 3 vuelos y ciudad no incluida en la agencia"
        ~: ciudadesConectadas
          [ (origen, "Rosario", 3.0),
            ("Tucuman", origen, 3.0),
            ("La Quiaca", origen, 3.0)
          ]
          "Brasilia"
        ~?= [],
      "ciudades conectadas con 5 vuelos diferentes, con dos que forman un ida y vuelta"
        ~: ciudadesConectadas
          [ (origen, "Rosario", 3.0),
            ("Tucuman", origen, 2.0),
            ("La Quiaca", origen, 8.4),
            (origen, "Tucuman", 2.45)
          ]
          origen
        ~?= ["Rosario", "Tucuman", "La Quiaca"],
      "ciudades conectadas con 3 vuelos diferentes, con uno que no es ni ida ni vuelta hacia Ciudad"
        ~: ciudadesConectadas
          [ (origen, "Rosario", 3.0),
            ("Tucuman", "Salta", 3.0),
            ("La Quiaca", origen, 3.0)
          ]
          origen
        ~?= ["Rosario", "La Quiaca"]
    ]

testModernizarFlota =
  test
    [ "modernizar flota sin vuelos" ~: modernizarFlota [] ~?= [],
      "modernizar flota con un vuelo"
        ~: modernizarFlota [("BsAs", "Rosario", 10.0)]
        ~?= [("BsAs", "Rosario", 9.0)],
      "modernizar flota con dos vuelos"
        ~: modernizarFlota [("BsAs", "Rosario", 57.0), ("La Plata", "Ciudad Chistosa", 20.0)]
        ~?= [("BsAs", "Rosario", 51.3), ("La Plata", "Ciudad Chistosa", 18.0)],
      "modernizar flota con tres vuelos"
        ~: modernizarFlota
          [ ("BsAs", "Rosario", 57.0),
            ("La Plata", "Ciudad Chistosa", 20.0),
            ("La Pampa", "La Rioja", 4.0)
          ]
        ~?= [ ("BsAs", "Rosario", 51.3),
              ("La Plata", "Ciudad Chistosa", 18.0),
              ("La Pampa", "La Rioja", 3.6)
            ]
    ]

testCiudadMasConectada =
  test
    [ "ciudad mas conectada sin vuelos" ~: ciudadMasConectada [] ~?= [],
      "ciudad mas conectada con un vuelo"
        ~: ciudadMasConectada [("BsAs", "Rosario", 10.0)]
        ~?= "Rosario",
      "ciudad mas conectada con dos vuelos"
        ~: ciudadMasConectada [("BsAs", "Rosario", 57.0), ("BsAs", "Ciudad Chistosa", 20.0)]
        ~?= "BsAs",
      "ciudad mas conectada con tres vuelos"
        ~: ciudadMasConectada
          [ ("BsAs", "Rosario", 57.0),
            ("La Plata", "Ciudad Chistosa", 20.0),
            ("La Pampa", "La Plata", 4.5)
          ]
        ~?= "La Plata"
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
        ~: duracionDelCaminoMasRapido [(origen, destino, 10.0)] origen destino
        ~?= 10.0,
      "duracion del camino mas rapido con una escala"
        ~: duracionDelCaminoMasRapido
          [ (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3)
          ]
          origen
          destino
        ~?= 7.3,
      "duracion del camino mas rapido con un vuelo directo y uno con escala, donde directo es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ (origen, destino, 3.4),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3)
          ]
          origen
          destino
        ~?= 3.4,
      "duracion del camino mas rapido con un vuelo directo y uno con escala, donde escala es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ (origen, destino, 9.8),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3)
          ]
          origen
          destino
        ~?= 7.3,
      "duracion del camino mas rapido con un vuelo directo y dos con escala, donde directo es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ (origen, destino, 4.8),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3),
            (origen, "La Plata", 2.0),
            ("La Plata", destino, 3.3)
          ]
          origen
          destino
        ~?= 4.8,
      "duracion del camino mas rapido con un vuelo directo y dos con escala, donde escala es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ (origen, destino, 9.8),
            ("Cordoba", destino, 10.0),
            (origen, "Rosario", 4.0),
            ("Rosario", destino, 3.3),
            (origen, "La Plata", 2.0),
            ("La Plata", destino, 3.3)
          ]
          origen
          destino
        ~?= 5.3
    ]

testPuedoVolverAOrigen =
  test
    [ "no puedo volver al origen sin vuelos"
        ~: puedoVolverAOrigen [] origen
        ~?= False,
      "no puedo volver al origen con un vuelo"
        ~: puedoVolverAOrigen [(origen, "La Plata", 0.0)] origen
        ~?= False,
      "puedo volver al origen con 2 vuelos, ida y vuelta directa"
        ~: puedoVolverAOrigen [(origen, "La Plata", 0.0), ("La Plata", origen, 0.0)] origen
        ~?= True,
      "puedo volver al origen con 3 vuelos, ida y vuelta con 1 escala"
        ~: puedoVolverAOrigen
          [ (origen, "La Plata", 0.0),
            ("La Plata", "San Juan", 0.0),
            ("San Juan", origen, 0.0)
          ]
          origen
        ~?= True,
      "puedo volver al origen con muchos vuelos, ida y vuelta con 1 escala y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          [ ("Rosario", "La Plata", 0.0),
            ("Cordoba", "La Rioja", 0.0),
            (origen, "La Rioja", 0.0), -- Salida 1
            ("Neuquen", "Misiones", 0.0),
            ("Rio Negro", "La Quiaca", 0.0),
            ("La Rioja", "Santa Fe", 0.0), -- Primera Escala 1
            (origen, "La Plata", 0.0), -- Salida 2
            ("La Plata", "San Juan", 0.0), -- Primera Escala 2
            ("Santa Fe", "Chubut", 0.0), -- Segunda Escala 1 -> Dead End
            ("San Juan", origen, 0.0) -- Llegada 2
          ]
          origen
        ~?= True,
      "no puedo volver al origen con dos posibilidades, muchos vuelos y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          [ ("Rosario", "La Plata", 0.0),
            ("Cordoba", "La Rioja", 0.0),
            (origen, "La Rioja", 0.0), -- Salida 1
            ("Neuquen", "Misiones", 0.0),
            ("Rio Negro", "La Quiaca", 0.0),
            ("La Rioja", "Santa Fe", 0.0), -- Primera Escala 1
            (origen, "La Plata", 0.0), -- Salida 2
            ("La Plata", "San Juan", 0.0), -- Primera Escala 2
            ("Santa Fe", "Chubut", 0.0), -- Segunda Escala 1 -> Dead End 1
            ("San Juan", "Rusia", 0.0) -- Segunda Escala 2 -> Dead End 2
          ]
          origen
        ~?= False,
      "puedo volver al origen con muchos vuelos, ida y vuelta con 2 escalas y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          -- Creo que los comentarios mas que sumar restan en este porque hay muchos dead ends
          -- No agregue todos, deje algunos
          [ ("Rosario", "La Plata", 0.0),
            ("Cordoba", "La Rioja", 0.0),
            ("La Plata", "La Rioja", 0.0), -- Primera Escala 2a
            ("Jujuy", origen, 0.0), -- Llegada 2b
            ("Santa Fe", "Ezeiza", 0.0),
            (origen, "La Rioja", 0.0), -- Salida 1
            ("Neuquen", "Misiones", 0.0),
            ("La Rioja", "Trenquelaunquen", 0.0), -- Primera Escala 1a y Segunda Escala 2a
            ("Rio Negro", "La Quiaca", 0.0),
            ("La Rioja", "Santa Fe", 0.0), -- Primera Escala 1b
            (origen, "La Plata", 0.0), -- Salida 2
            ("La Plata", "San Juan", 0.0), -- Primera Escala 2b
            ("Santa Fe", "Chubut", 0.0), -- Segunda Escala 1b y Tercera Escala 2a -> Dead End
            ("Trenquelaunquen", "Rusia", 0.0), -- Segunda Escala 1a -> Dead End
            ("San Juan", "Jujuy", 0.0), -- Segunda Escala 2b
            ("Ezeiza", "España", 0.0)
          ]
          origen
        ~?= True,
      "no puedo volver al origen con muchos vuelos, ida y vuelta con 2 escalas y dead ends en medio estorbando"
        ~: puedoVolverAOrigen
          [ ("Rosario", "La Plata", 0.0),
            ("Cordoba", "La Rioja", 0.0),
            ("La Plata", "La Rioja", 0.0),
            ("Jujuy", "Salta", 0.0),
            ("Santa Fe", "Ezeiza", 0.0),
            (origen, "La Rioja", 0.0),
            ("Neuquen", "Misiones", 0.0),
            ("La Rioja", "Trenquelaunquen", 0.0),
            ("Rio Negro", "La Quiaca", 0.0),
            ("La Rioja", "Santa Fe", 0.0),
            (origen, "La Plata", 0.0),
            ("La Plata", "San Juan", 0.0),
            ("Santa Fe", "Chubut", 0.0),
            ("Trenquelaunquen", "Rusia", 0.0),
            ("San Juan", "Jujuy", 0.0),
            ("Ezeiza", "España", 0.0)
          ]
          origen
        ~?= False
    ]
