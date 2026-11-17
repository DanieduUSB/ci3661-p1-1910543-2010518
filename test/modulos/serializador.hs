module Serializador (testsSerializador) where

import Test.HUnit
import Motor.Tipos
import Motor.Serializador
import Datos.EstadoPrueba

testsSerializador :: Test
testsSerializador = TestList
    [ TestLabel "Serializar y deserializar" testIdaVuelta
    , TestLabel "Mundo incorrecto"          testMundoIncorrecto
    , TestLabel "Contenido invalido"        testContenidoInvalido
    , TestLabel "Sala inexistente"          testSalaInexistente
    ]

-- Serializar y deserializar conserva el estado
testIdaVuelta :: Test
testIdaVuelta = TestCase $ do
    let resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            (serializarEstado estadoPrueba)

    assertEqual "Debe recuperar el estado original" (Right estadoPrueba) resultado


-- Rechazar una partida de otro mundo
testMundoIncorrecto :: Test
testMundoIncorrecto = TestCase $ do
    let mundoDistinto = (estadoMundo estadoPrueba) { mundoTitulo = "Otro mundo" }

        resultado = deserializarEstado mundoDistinto (serializarEstado estadoPrueba)

    assertEqual "Debe rechazar otro mundo"
        (Left "La partida pertenece a otro mundo")
        resultado


-- Rechazar contenido mal formado
testContenidoInvalido :: Test
testContenidoInvalido = TestCase $ do
    let resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            "contenido completamente invalido"

    assertEqual "Debe rechazar contenido invalido"
        (Left "Formato de partida invalido")
        resultado


-- Rechazar una sala que no existe
testSalaInexistente :: Test
testSalaInexistente = TestCase $ do
    let estadoInvalido = estadoPrueba { salaActual = IdSala "sala_fantasma" }

        resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            (serializarEstado estadoInvalido)

    assertEqual "Debe rechazar una sala inexistente"
        (Left "La sala actual no existe")
        resultado