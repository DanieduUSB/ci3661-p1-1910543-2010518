module Nucleo (testsNucleo) where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)

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
    , TestLabel "Tomar dos veces"                   testTomarDosVeces
    , TestLabel "Soltar objeto ausente"             testSoltarAusente
    , TestLabel "Examinar objeto en sala"           testExaminarSala
    , TestLabel "Examinar objeto en inventario"     testExaminarInventario
    , TestLabel "Examinar objeto inaccesible"       testExaminarInaccesible
    , TestLabel "Peso exacto"                       testPesoExacto
    , TestLabel "Movimiento fallido"                testMovimientoFallido
    , TestLabel "Peso después de soltar"            testPesoDespuesDeSoltar
    , TestLabel "Secuencia de comandos ganadora"    testSecuenciaGanadora
    , TestLabel "Salida bloqueada y recuperación"   testSalidaBloqueadaRecuperacion
    , TestLabel "Capacidad y recuperación"          testCapacidad
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


testTomarDosVeces :: Test
testTomarDosVeces = TestCase $ do
    let objeto = IdObjeto "llave_magnetica"
        (_, estado1) = ejecutar (Tomar objeto) estadoPrueba
        (_, estado2) = ejecutar (Tomar objeto) estado1

    assertEqual "El segundo intento no modifica el estado" estado1 estado2

    assertEqual "El inventario contiene un solo objeto" 1 (Set.size (inventario estado2))


testSoltarAusente :: Test
testSoltarAusente = TestCase $ do
    let (_, nuevoEstado) =
            ejecutar (Soltar (IdObjeto "llave")) estadoPrueba

    assertEqual "No puede soltar un objeto que no posee" estadoPrueba nuevoEstado


testExaminarSala :: Test
testExaminarSala = TestCase $ do
    let (mensajes, nuevo) =
            ejecutar (Examinar (IdObjeto "llave_magnetica")) estadoPrueba

    assertBool "Muestra la descripción del objeto"
        (any (isInfixOf "cerradura magnética") mensajes)

    assertEqual "Examinar no modifica el estado" estadoPrueba nuevo


testExaminarInventario :: Test
testExaminarInventario = TestCase $ do
    let (_, conLlave) = ejecutar (Tomar (IdObjeto "llave_magnetica")) estadoPrueba

        (mensajes, nuevo) = ejecutar (Examinar (IdObjeto "llave_magnetica")) conLlave

    assertBool "Muestra la descripción del objeto del inventario"
        (any (isInfixOf "cerradura magnética") mensajes)

    assertEqual "Examinar no modifica el estado" conLlave nuevo


testExaminarInaccesible :: Test
testExaminarInaccesible = TestCase $ do
    let (mensajes, nuevo) = ejecutar (Examinar (IdObjeto "paracaidas")) estadoPrueba

    assertBool "No muestra la descripción del objeto inaccesible"
        (not (any (isInfixOf "Parece funcional") mensajes))

    assertEqual "El estado permanece igual" estadoPrueba nuevo


testPesoExacto :: Test
testPesoExacto = TestCase $ do
    let estado = estadoPrueba
            { salaActual = IdSala "apto_1_b"
            , inventario = Set.fromList
                [ IdObjeto "llave_magnetica"
                , IdObjeto "llave"
                , IdObjeto "paracaidas"
                ]
            }

        (_, nuevo) = ejecutar (Tomar (IdObjeto "palanca")) estado

    assertEqual "El peso del inventario alcanza la capacidad"
        (mundoCapacidad (estadoMundo nuevo))
        (pesoInventario nuevo)

    assertBool "Permite tomar el objeto al alcanzar exactamente la capacidad"
        (Set.member (IdObjeto "palanca") (inventario nuevo))


testMovimientoFallido :: Test
testMovimientoFallido = TestCase $ do
    let (_, nuevo) = ejecutar (Ir Norte) estadoPrueba

    assertEqual "No cambia la sala actual" (IdSala "calle") (salaActual nuevo)

    assertBool "No desbloquea ninguna salida" (Set.null (salidasDesbloqueadas nuevo))


testPesoDespuesDeSoltar :: Test
testPesoDespuesDeSoltar = TestCase $ do
    let inicial = estadoPrueba
            { salaActual = IdSala "apto_1_b"
            , inventario = Set.fromList
                [ IdObjeto "llave_magnetica"
                , IdObjeto "llave"
                , IdObjeto "paracaidas"
                ]
            }

        -- Inventario inicial: peso 8
        (_, conPalanca) = ejecutar (Tomar (IdObjeto "palanca")) inicial

        -- Inventario: peso 11
        (_, sinParacaidas) = ejecutar (Soltar (IdObjeto "paracaidas")) conPalanca

        -- Inventario: peso 7
        (_, conParacaidas) = ejecutar (Tomar (IdObjeto "paracaidas")) sinParacaidas

    assertEqual "Peso inicial" 8 (pesoInventario inicial)

    assertEqual "Peso al tomar la palanca" 11 (pesoInventario conPalanca)

    assertEqual "Peso despues de soltar el paracaidas" 7 (pesoInventario sinParacaidas)

    assertEqual "Peso despues de recuperar el paracaidas" 11 (pesoInventario conParacaidas)

    assertEqual "Inventario final igual a la capacidad"
        (mundoCapacidad (estadoMundo conParacaidas))
        (pesoInventario conParacaidas)


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


testCapacidad :: Test
testCapacidad = TestCase $ do
    let inicio = estadoInicial mundoMetaSinRequisito
        
        antes = ejecutarSecuencia inicio
            [ Tomar (IdObjeto "llave_magnetica")
            , Ir Norte
            , Ir Arriba
            , Ir Arriba
            , Ir Este
            , Tomar (IdObjeto "llave")
            , Ir Oeste
            , Ir Abajo
            , Ir Este
            ]

        rechazado = ejecutarSecuencia antes
            [Tomar (IdObjeto "paracaidas")]

        final = ejecutarSecuencia rechazado
            [ Soltar (IdObjeto "llave_magnetica")
            , Tomar (IdObjeto "paracaidas")
            ]

    assertEqual "El peso excesivo no modifica el estado" antes rechazado

    assertBool "Puede recogerlo después de soltar otro objeto"
        (Set.member (IdObjeto "paracaidas") (inventario final))

    assertBool "La llave magnética ya no está en el inventario"
        (Set.notMember (IdObjeto "llave_magnetica") (inventario final))


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

-- Función auxiliar pesoInventario
pesoInventario :: Estado -> Int
pesoInventario estado =
    sum (map obtenerPeso (Set.toList (inventario estado)))
    where
        objetos = mundoObjetos (estadoMundo estado)

        obtenerPeso idObj =
            case Map.lookup idObj objetos of
                Just [obj] -> objetoPeso obj
                _          -> 0