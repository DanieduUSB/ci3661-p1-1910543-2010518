module Nucleo (testsNucleo) where

import Test.HUnit

testsNucleo :: Test
testsNucleo = TestList []

-- Cada prueba individual tiene una de las siguientes estructuras:
-- TestCase $ assertEqual "descripción" resultadoEsperado resultadoReal
--  Ejemplo: TestCase $ assertEqual "Suma de dos números" 3 (sumar 1 2)
-- TestCase $ assertBool "descripcion" expresionBooleana
--  Ejemplo: TestCase $ assertBool "El número 1 pertenece a la lista" (elem 1 [1, 2])

-- Para varias pruebas, usa la lista de la función testsNucleo. Ejemplo:
--testsNucleo = TestList
--    [ TestCase $ assertEqual "Suma de dos números" (3 :: Int) (1 + 2)
--    , TestCase $ assertBool "El número 1 pertenece a la lista" (elem (1 :: Int) [1, 2])
--    ]

-- Nota: (3 :: Int) y (1 :: Int) son porque Haskell no puede inferir si el tipo es Int o Integer
-- por lo que da warning al hacer stack test, pero en general no es obligatorio ponerle el tipo