module Serializador (testsSerializador) where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Motor.Tipos
import Motor.Serializador
import Datos.EstadoPrueba

testsSerializador :: Test
testsSerializador = TestList
    [ TestLabel "Serializar y deserializar" testIdaVuelta
    , TestLabel "Mundo incorrecto"          testMundoIncorrecto
    , TestLabel "Contenido invalido"        testContenidoInvalido
    , TestLabel "Sala inexistente"          testSalaInexistente
    , TestLabel "Objeto duplicado"          testObjetoDuplicado
    , TestLabel "Objeto inexistente"        testObjetoInexistente
    , TestLabel "Salida inexistente"        testSalidaInexistente
    , TestLabel "Objeto desaparecido"       testObjetoDesaparecido
    , TestLabel "Sala guardada inexistente" testSalaGuardadaInexistente
    , TestLabel "Inventario excedido"       testInventarioExcedido
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


-- Rechazar objetos duplicados en una misma sala
testObjetoDuplicado :: Test
testObjetoDuplicado = TestCase $ do
    let texto = serializarEstado estadoPrueba

        textoInvalido = unlines
            [ if linea == "calle: llave_magnetica"
                then "calle: llave_magnetica,llave_magnetica"
                else linea
            | linea <- lines texto
            ]

        resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            textoInvalido

    assertEqual "Debe rechazar objetos duplicados"
        (Left "Hay objetos duplicados en la partida")
        resultado


-- Rechazar objetos inexistentes
testObjetoInexistente :: Test
testObjetoInexistente = TestCase $ do
    let estadoInvalido = estadoPrueba
            { inventario = Set.singleton (IdObjeto "objeto_fantasma") }

        resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            (serializarEstado estadoInvalido)

    assertEqual "Debe rechazar objetos inexistentes"
        (Left "La partida contiene objetos inexistentes")
        resultado


-- Rechazar salidas inexistentes
testSalidaInexistente :: Test
testSalidaInexistente = TestCase $ do
    let estadoInvalido = estadoPrueba
            { salidasDesbloqueadas =
                Set.singleton (IdSala "calle", Oeste)
            }

        resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            (serializarEstado estadoInvalido)

    assertEqual "Debe rechazar salidas inexistentes"
        (Left "La partida contiene salidas inexistentes")
        resultado


-- Rechazar objetos desaparecidos
testObjetoDesaparecido :: Test
testObjetoDesaparecido = TestCase $ do
    let estadoInvalido = estadoPrueba
            { objetosPorSala = Map.adjust
                (Set.delete (IdObjeto "llave_magnetica"))
                (IdSala "calle")
                (objetosPorSala estadoPrueba)
            }

        resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            (serializarEstado estadoInvalido)

    assertEqual "Debe rechazar objetos desaparecidos"
        (Left "Faltan objetos en la partida")
        resultado


-- Rechazar salas inexistentes
testSalaGuardadaInexistente :: Test
testSalaGuardadaInexistente = TestCase $ do
    let texto = serializarEstado estadoPrueba

        textoInvalido = unlines
            [ if linea == "calle: llave_magnetica"
                then "sala_fantasma: llave_magnetica"
                else linea
            | linea <- lines texto
            ]

        resultado = deserializarEstado
            (estadoMundo estadoPrueba)
            textoInvalido

    assertEqual "Debe rechazar salas inexistentes"
        (Left "La partida contiene salas inexistentes")
        resultado


-- Rechazar inventarios que exceden la capacidad
testInventarioExcedido :: Test
testInventarioExcedido = TestCase $ do
    let mundo = (estadoMundo estadoPrueba)
            { mundoCapacidad = 3 }

        objetosInventario = Set.fromList
            [ IdObjeto "llave_magnetica"
            , IdObjeto "paracaidas"
            ]

        estadoInvalido = estadoPrueba
            { estadoMundo = mundo
            , inventario = objetosInventario
            , objetosPorSala = Map.map
                (\objetos -> Set.difference objetos objetosInventario)
                (objetosPorSala estadoPrueba)
            }

        resultado = deserializarEstado
            mundo
            (serializarEstado estadoInvalido)

    assertEqual "Debe rechazar inventarios que superen la capacidad"
        (Left "El inventario supera la capacidad")
        resultado
