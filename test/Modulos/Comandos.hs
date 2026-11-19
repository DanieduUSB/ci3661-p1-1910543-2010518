module Comandos (testsComandos) where

import Test.HUnit
import Motor.Tipos
import Motor.Comandos

testsComandos :: Test
testsComandos = TestList
    [ TestCase $ assertEqual
        "ir norte"
        (Just (Ir Norte))
        (leerComando "ir norte")

    , TestCase $ assertEqual
        "ir arriba"
        (Just (Ir Arriba))
        (leerComando "ir arriba")

    , TestCase $ assertEqual
        "ir con dirección no existente"
        (Just (Ir Invalida))
        (leerComando "ir calacras")

    , TestCase $ assertEqual
        "mirar"
        (Just Mirar)
        (leerComando "mirar")

    , TestCase $ assertEqual
        "tomar id_obj (Comando con argumento)"
        (Just (Tomar (IdObjeto "carnet")))
        (leerComando "tomar carnet")

    , TestCase $ assertEqual
        "Alias coger coso"
        (Just (Tomar (IdObjeto "coso")))
        (leerComando "coger coso")

    , TestCase $ assertEqual
        "comando en mayúsculas"
        (Just Salir)
        (leerComando "SALIR")

    , TestCase $ assertEqual
        "comando con argumento mayúsculas y minúsculas"
        (Just (Ir Este))
        (leerComando "iR ESte")
    
    , TestCase $ assertEqual
        "comando inexistente"
        Nothing
        (leerComando "hackear")
    
    , TestCase $ assertEqual
        "comando con argumento, sin colocar argumento"
        Nothing
        (leerComando "soltar")
    
    , TestCase $ assertEqual
        "comando con argumento, sin colocar argumento, con espacio después del comando"
        Nothing
        (leerComando "examinar ")
    
    , TestCase $ assertEqual
        "comando con argumento, con mas de un argumento"
        Nothing
        (leerComando "soltar id objeto")
    
    , TestCase $ assertEqual 
        "direccion norte sin ir"
        (Just (Ir Norte)) (leerComando "norte")

    , TestCase $ assertEqual 
        "palabra desconocida sola sigue siendo invalida"
        Nothing (leerComando "diagonal")
    ]

-- Cada prueba individual tiene una de las siguientes estructuras:
-- TestCase $ assertEqual "descripción" resultadoEsperado resultadoReal
--  Ejemplo: TestCase $ assertEqual "Suma de dos números" 3 (sumar 1 2)
-- TestCase $ assertBool "descripcion" expresionBooleana
--  Ejemplo: TestCase $ assertBool "El número 1 pertenece a la lista" (elem 1 [1, 2])

-- Para varias pruebas, usa la lista de la función testsComandos. Ejemplo:
--testsComandos = TestList
--    [ TestCase $ assertEqual "Suma de dos números" (3 :: Int) (1 + 2)
--    , TestCase $ assertBool "El número 1 pertenece a la lista" (elem (1 :: Int) [1, 2])
--    ]

-- Nota: (3 :: Int) y (1 :: Int) son porque Haskell no puede inferir si el tipo es Int o Integer
-- por lo que da warning al hacer stack test, pero en general no es obligatorio ponerle el tipo