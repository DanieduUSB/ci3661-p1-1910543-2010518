module Nucleo (testsNucleo) where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Motor.Tipos
import Motor.Nucleo
import Datos.MundoPrueba
import Datos.EstadoPrueba

testsNucleo :: Test
testsNucleo = TestList
    [ TestLabel "Estado inicial"                    testEstadoInicial
    , TestLabel "Inventario vacío"                  testInventarioVacio
    , TestLabel "Tomar objeto"                      testTomar
    , TestLabel "Tomar objeto inexistente en sala"  testTomarAusente
    , TestLabel "Superar capacidad"                 testPesoExcedido
    , TestLabel "Movimiento normal"                 testMovimiento
    , TestLabel "Salida inexistente"                testSalidaInexistente
    , TestLabel "Salida bloqueada"                  testSalidaBloqueada
    , TestLabel "Abrir salida"                      testAbrirSalida
    , TestLabel "Desbloqueo permanente"             testDesbloqueoPermanente
    , TestLabel "Salidas independientes"            testSalidasIndependientes
    , TestLabel "Soltar objeto"                     testSoltar
    , TestLabel "Victoria sin requisito"            testVictoriaSinRequisito
    , TestLabel "Victoria con requisito"            testVictoriaConRequisito
    , TestLabel "Meta incompleta"                   testMetaIncompleta
    , TestLabel "Secuencia de comandos ganadora"    testSecuenciaGanadora
    , TestLabel "Salida bloqueada y recuperación"   testSalidaBloqueadaRecuperacion
    ]

testEstadoInicial :: Test
testEstadoInicial = TestCase $ do
    assertEqual "Sala inicial"
        (IdSala "calle")
        (salaActual estadoPrueba)
    
    assertBool "Inventario inicialmente vacío"
        (Set.null (inventario estadoPrueba))
    
    assertBool "No hay salidas desbloqueadas"
        (Set.null (salidasDesbloqueadas estadoPrueba))


testInventarioVacio :: Test
testInventarioVacio = TestCase $
    assertEqual "Inventario vacío"
        (["Inventario:", "(Vacío)"], estadoPrueba)
        (ejecutar Inventario estadoPrueba)


testTomar :: Test
testTomar = TestCase $ do
    let (_, nuevoEstado) = ejecutar (Tomar (IdObjeto "llave_magnetica")) estadoPrueba

    assertBool "El objeto pasa al inventario"
        (Set.member (IdObjeto "llave_magnetica") (inventario nuevoEstado))

    assertBool "El objeto desaparece de la sala"
        (Set.notMember (IdObjeto "llave_magnetica")
            (objetosEnSalaActual nuevoEstado))


testTomarAusente :: Test
testTomarAusente = TestCase $ do
    let (_, nuevoEstado) =
            ejecutar (Tomar (IdObjeto "paracaidas")) estadoPrueba

    assertEqual "El estado no cambia" estadoPrueba nuevoEstado


testPesoExcedido :: Test
testPesoExcedido = TestCase $ do
    let mundo = (estadoMundo estadoPrueba)
            { mundoCapacidad = 1 }
        estado = estadoPrueba { estadoMundo = mundo }
        (_, nuevoEstado) =
            ejecutar (Tomar (IdObjeto "llave_magnetica")) estado

    assertEqual "No puede tomar un objeto demasiado pesado" estado nuevoEstado


testMovimiento :: Test
testMovimiento = TestCase $ do
    let estado = estadoPrueba { salaActual = IdSala "pb" }
        (_, nuevoEstado) = ejecutar (Ir Arriba) estado

    assertEqual "Llega al primer piso" (IdSala "piso_1") (salaActual nuevoEstado)


testSalidaInexistente :: Test
testSalidaInexistente = TestCase $ do
    let (_, nuevoEstado) = ejecutar (Ir Oeste) estadoPrueba

    assertEqual "No cambia de sala" estadoPrueba nuevoEstado


testSalidaBloqueada :: Test
testSalidaBloqueada = TestCase $ do
    let (_, nuevoEstado) = ejecutar (Ir Norte) estadoPrueba

    assertEqual "No puede entrar sin llave magnética" estadoPrueba nuevoEstado


testAbrirSalida :: Test
testAbrirSalida = TestCase $ do
    let estado = estadoConLlaveMagnetica
        (_, nuevoEstado) = ejecutar (Ir Norte) estado

    assertEqual "Va de la calle a PB" (IdSala "pb") (salaActual nuevoEstado)

    assertBool "La salida queda desbloqueada"
        (Set.member (IdSala "calle", Norte) (salidasDesbloqueadas nuevoEstado))


testDesbloqueoPermanente :: Test
testDesbloqueoPermanente = TestCase $ do
    let (_, abierto) = ejecutar (Ir Norte) estadoConLlaveMagnetica
        estado = abierto
            { salaActual = IdSala "calle"
            , inventario = Set.empty
            }
        (_, nuevoEstado) = ejecutar (Ir Norte) estado

    assertEqual "Puede cruzar sin conservar la llave" (IdSala "pb") (salaActual nuevoEstado)


testSalidasIndependientes :: Test
testSalidasIndependientes = TestCase $ do
    let (_, abierto) = ejecutar (Ir Norte) estadoConLlaveMagnetica
        estado = abierto { inventario = Set.empty }
        (_, nuevoEstado) = ejecutar (Ir Sur) estado

    assertEqual "La salida de regreso sigue bloqueada"
        (IdSala "pb")
        (salaActual nuevoEstado)


testSoltar :: Test
testSoltar = TestCase $ do
    let (_, nuevoEstado) =
            ejecutar (Soltar (IdObjeto "llave_magnetica")) estadoConLlaveMagnetica

    assertBool "El objeto sale del inventario"
        (Set.notMember (IdObjeto "llave_magnetica") (inventario nuevoEstado))

    assertBool "El objeto aparece en la sala"
        (Set.member (IdObjeto "llave_magnetica") (objetosEnSalaActual nuevoEstado))


testVictoriaSinRequisito :: Test
testVictoriaSinRequisito = TestCase $ do
    let estado = (estadoInicial mundoMetaSinRequisito)
            { salaActual = IdSala "piso_2"
            , inventario = Set.singleton (IdObjeto "palanca")
            }
        (mensaje, nuevoEstado) = ejecutar (Ir Arriba) estado

    assertEqual "Mensaje de victoria" ["*** ¡Has ganado! ***"] mensaje

    assertBool "Se completa la meta" (juegoTerminado nuevoEstado)


testVictoriaConRequisito :: Test
testVictoriaConRequisito = TestCase $ do
    let estado = estadoPrueba
            { salaActual = IdSala "piso_2"
            , inventario = Set.fromList
                [ IdObjeto "paracaidas"
                , IdObjeto "palanca"
                ]
            }
        (mensaje, nuevoEstado) = ejecutar (Ir Arriba) estado
    
    assertEqual "Mensaje de victoria" ["*** ¡Has ganado! ***"] mensaje

    assertBool "Se completa la meta llevando paracaídas" (juegoTerminado nuevoEstado)


testMetaIncompleta :: Test
testMetaIncompleta = TestCase $ do
    let estado = estadoPrueba
            { salaActual = IdSala "piso_2"
            , inventario = Set.singleton (IdObjeto "palanca")
            }
        (_, nuevoEstado) = ejecutar (Ir Arriba) estado

    assertBool "No gana sin paracaídas" (not (juegoTerminado nuevoEstado))

    assertEqual "Puede entrar a la azotea" (IdSala "azotea") (salaActual nuevoEstado)


testSecuenciaGanadora :: Test
testSecuenciaGanadora = TestCase $ do
    let final = ejecutarSecuencia estadoPrueba
            [ Tomar (IdObjeto "llave_magnetica")
            , Ir Norte
            , Soltar (IdObjeto "llave_magnetica")
            , Ir Arriba
            , Ir Arriba
            , Ir Este
            , Tomar (IdObjeto "llave")
            , Ir Oeste
            , Ir Abajo
            , Ir Este
            , Soltar (IdObjeto "llave")
            , Tomar (IdObjeto "paracaidas")
            , Ir Oeste
            , Ir Oeste
            , Tomar (IdObjeto "palanca")
            , Ir Este
            , Ir Arriba
            , Ir Arriba
            ]
    
    assertEqual "Llega a la azotea" (IdSala "azotea") (salaActual final)

    assertBool "Tiene el paracaídas"
        (Set.member (IdObjeto "paracaidas") (inventario final))
    
    assertBool "La partida terminó" (juegoTerminado final)


testSalidaBloqueadaRecuperacion :: Test
testSalidaBloqueadaRecuperacion = TestCase $ do
    let bloqueado = ejecutarSecuencia estadoPrueba [Ir Norte]

        final = ejecutarSecuencia bloqueado
            [ Tomar (IdObjeto "llave_magnetica")
            , Ir Norte
            ]

    assertEqual "No puede entrar sin la llave magnética"
        (IdSala "calle")
        (salaActual bloqueado)

    assertEqual "Puede entrar después de recoger la llave magnética"
        (IdSala "pb")
        (salaActual final)

    assertBool "La salida queda desbloqueada"
        (Set.member (IdSala "calle", Norte)
            (salidasDesbloqueadas final))


-- Función auxiliar objetosEnSalaActual
objetosEnSalaActual :: Estado -> Set.Set IdObjeto
objetosEnSalaActual estado =
    Map.findWithDefault Set.empty
        (salaActual estado)
        (objetosPorSala estado)

-- Función auxiliar ejecutarSecuencia
-- Ejecuta una lista de comandos consecutivamente
ejecutarSecuencia :: Estado -> [Comando] -> Estado
ejecutarSecuencia = foldl (\estado comando -> snd (ejecutar comando estado))