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
      "sePuedeLlegar" ~: testSePuedeLLegar
    ]

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
    [ "ciudades conectadas sin vuelos" ~: ciudadesConectadas [] "BsAs" ~?= [],
      "ciudades conectadas con un elemento"
        ~: ciudadesConectadas [("BsAs", "Rosario", 5.0)] "BsAs"
        ~?= ["Rosario"],
      "ciudades conectadas con 2 vuelos, mismo origen, buscando ese origen"
        ~: ciudadesConectadas [("BsAs", "Rosario", 3.0), ("BsAs", "Tucuman", 3.0)] "BsAs"
        ~?= ["Rosario", "Tucuman"],
      "ciudades conectadas con 3 vuelos, objetivo intercalado"
        ~: ciudadesConectadas
          [ ("BsAs", "Rosario", 3.0),
            ("Tucuman", "BsAs", 3.0),
            ("La Quiaca", "BsAs", 3.0)
          ]
          "BsAs"
        ~?= ["Rosario", "Tucuman", "La Quiaca"],
      "ciudades conectadas con 3 vuelos y ciudad no incluida en la agencia"
        ~: ciudadesConectadas
          [ ("BsAs", "Rosario", 3.0),
            ("Tucuman", "BsAs", 3.0),
            ("La Quiaca", "BsAs", 3.0)
          ]
          "Brasilia"
        ~?= [],
      "ciudades conectadas con 5 vuelos diferentes, con dos que forman un ida y vuelta"
        ~: ciudadesConectadas
          [ ("BsAs", "Rosario", 3.0),
            ("Tucuman", "BsAs", 2.0),
            ("La Quiaca", "BsAs", 8.4),
            ("BsAs", "Tucuman", 2.45)
          ]
          "BsAs"
        ~?= ["Rosario", "Tucuman", "La Quiaca"],
      "ciudades conectadas con 3 vuelos diferentes, con uno que no es ni ida ni vuelta hacia Ciudad"
        ~: ciudadesConectadas
          [ ("BsAs", "Rosario", 3.0),
            ("Tucuman", "Salta", 3.0),
            ("La Quiaca", "BsAs", 3.0)
          ]
          "BsAs"
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

origen :: String
origen = "BsAs"

destino :: String
destino = "Tucuman"

testSePuedeLLegar =
  test
    [ "no se puede llegar sin vuelos" ~: sePuedeLlegar [] origen destino ~?= False,
      "se puede llegar con un vuelo que matchea"
        ~: sePuedeLlegar [("BsAs", "Tucuman", 10.0)] origen destino
        ~?= True,
      "no se puede llegar con un vuelo que no matchea"
        ~: sePuedeLlegar [("BsAs", "Rosario", 10.0)] origen destino
        ~?= False,
      "se puede llegar sin escala con lista con multiples vuelos"
        ~: sePuedeLlegar
          [ ("BsAs", "Rosario", 10.0),
            ("Cordoba", "Tucuman", 10.0),
            ("BsAs", "Tucuman", 10.0),
            ("Rosario", "Tucuman", 10.0)
          ]
          origen
          destino
        ~?= True,
      "se puede llegar con maximo una escala con multiples vuelos, encontrando primero el origen"
        ~: sePuedeLlegar
          [ ("BsAs", "Rosario", 10.0),
            ("Cordoba", "Tucuman", 10.0),
            ("BsAs", "La Plata", 10.0),
            ("Konoha", "Ciudad Chistosa", 10.0),
            ("Rosario", "Brasil", 10.0),
            ("Rosario", "Tucuman", 10.0),
            ("Rosario", "Bulnes", 10.0)
          ]
          origen
          destino
        ~?= True,
      "se puede llegar con maximo una escala con multiples vuelos, encontrando primero el destino"
        ~: sePuedeLlegar
          [ ("Rosario", "Tucuman", 10.0),
            ("Cordoba", "Tucuman", 10.0),
            ("BsAs", "La Plata", 10.0),
            ("Konoha", "Ciudad Chistosa", 10.0),
            ("Rosario", "Brasil", 10.0),
            ("BsAs", "Rosario", 10.0),
            ("Rosario", "Bulnes", 10.0)
          ]
          origen
          destino
        ~?= True
    ]

testDuracionMasCorta =
  test
    [ "duracion del camino mas rapido con un vuelo"
        ~: duracionDelCaminoMasRapido [("BsAs", "Tucuman", 10.0)] origen destino
        ~?= 10.0,
      "duracion del camino mas rapido con un vuelo directo y uno con escala, donde directo es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ ("BsAs", "Tucuman", 3.4),
            ("Cordoba", "Tucuman", 10.0),
            ("BsAs", "Rosario", 4.0),
            ("Rosario", "Tucuman", 3.3)
          ]
          origen
          destino
        ~?= 3.4,
      "duracion del camino mas rapido con un vuelo directo y uno con escala, donde escala es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ ("BsAs", "Tucuman", 9.8),
            ("Cordoba", "Tucuman", 10.0),
            ("BsAs", "Rosario", 4.0),
            ("Rosario", "Tucuman", 3.3)
          ]
          origen
          destino
        ~?= 7.3,
      "duracion del camino mas rapido con un vuelo directo y dos con escala, donde directo es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ ("BsAs", "Tucuman", 4.8),
            ("Cordoba", "Tucuman", 10.0),
            ("BsAs", "Rosario", 4.0),
            ("Rosario", "Tucuman", 3.3),
            ("BsAs", "La Plata", 2.0),
            ("La Plata", "Tucuman", 3.3)
          ]
          origen
          destino
        ~?= 4.8,
      "duracion del camino mas rapido con un vuelo directo y dos con escala, donde escala es mas rapido"
        ~: duracionDelCaminoMasRapido
          [ ("BsAs", "Tucuman", 9.8),
            ("Cordoba", "Tucuman", 10.0),
            ("BsAs", "Rosario", 4.0),
            ("Rosario", "Tucuman", 3.3),
            ("BsAs", "La Plata", 2.0),
            ("La Plata", "Tucuman", 3.3)
          ]
          origen
          destino
        ~?= 5.3
    ]
