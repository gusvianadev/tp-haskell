import Data.List
import Solucion
import Test.HUnit

runAllTests = runTestTT allTests

allTests =
  test
    [ "vuelosValidos" ~: testVuelosValidos,
      "ciudadesConectadas" ~: testCiudadesConectadas
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
