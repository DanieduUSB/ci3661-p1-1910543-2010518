module Nucleo (testsNucleo) where

import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Motor.Tipos
import Motor.Nucleo
import Datos.EstadoPrueba

testsNucleo :: Test
testsNucleo = TestList
    [ TestLabel "Estado inicial" testEstadoInicial
    , TestLabel "Inventario vacío" testInventarioVacio
    ]

testEstadoInicial :: Test
testEstadoInicial = TestCase $ do
    assertEqual "Sala inicial"
        (IdSala "calle")
        (salaActual estadoPruebaConRequisito)
    
    assertBool "Inventario inicialmente vacío"
        (Set.null (inventario estadoPruebaConRequisito))
    
    assertBool "No hay salidas desbloqueadas"
        (Set.null (salidasDesbloqueadas estadoPruebaConRequisito))


testInventarioVacio :: Test
testInventarioVacio = TestCase $
    assertEqual "Inventario vacío"
        (["Inventario:", "(Vacío)"], estadoPruebaConRequisito)
        (ejecutar Inventario estadoPruebaConRequisito)


