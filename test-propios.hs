import Data.List
import Solucion
import Test.HUnit

-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

allTests =
  test
    ["vuelosValidos" ~: testsEjvuelosValidos]

testsEjvuelosValidos =
  test ["vuelos válido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True]
