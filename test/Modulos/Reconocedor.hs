-- Pruebas unitarias de los reconocedores ReadS.
module Reconocedor (testsReconocedor) where

import Test.HUnit
import Motor.Tipos
import Motor.Reconocedor
import qualified Data.Map.Strict as Map

testsReconocedor :: Test
testsReconocedor = TestList
    [ TestLabel "MUNDO acepta texto libre" $ TestCase $
        assertEqual "titulo" (Just "Mi aventura")
            (reconocerCompleto lineaMundo "MUNDO: Mi aventura")

    , TestLabel "MUNDO rechaza titulo vacio" $ TestCase $
        assertEqual "titulo vacio" (Nothing :: Maybe String)
            (reconocerCompleto lineaMundo "MUNDO: ")

    , TestLabel "INICIO reconoce identificador" $ TestCase $
        assertEqual "identificador" (Just (IdSala "plaza"))
            (reconocerCompleto lineaInicio "INICIO: plaza")

    , TestLabel "INICIO rechaza mayusculas" $ TestCase $
        assertEqual "id invalido" (Nothing :: Maybe IdSala)
            (reconocerCompleto lineaInicio "INICIO: Plaza")

    , TestLabel "INICIO rechaza texto sobrante" $ TestCase $
        assertEqual "entrada incompleta" (Nothing :: Maybe IdSala)
            (reconocerCompleto lineaInicio "INICIO: plaza principal")

    , TestLabel "META de llegar" $ TestCase $
        assertEqual "llegar" (Just (Llegar (IdSala "biblioteca")))
            (reconocerCompleto lineaMeta "META: llegar a biblioteca")

    , TestLabel "META de llevar" $ TestCase $
        assertEqual "llevar" (Just (Llevar (IdObjeto "carnet") (IdSala "biblioteca")))
            (reconocerCompleto lineaMeta "META: llevar carnet a biblioteca")

    , TestLabel "META mal formada" $ TestCase $
        assertEqual "falta a" (Nothing :: Maybe Meta)
            (reconocerCompleto lineaMeta "META: llevar carnet biblioteca")

    , TestLabel "CAPACIDAD positiva" $ TestCase $
        assertEqual "capacidad" (Just (5 :: Int))
            (reconocerCompleto lineaCapacidad "CAPACIDAD: 5")

    , TestLabel "CAPACIDAD cero invalida" $ TestCase $
        assertEqual "capacidad cero" (Nothing :: Maybe Int)
            (reconocerCompleto lineaCapacidad "CAPACIDAD: 0")

    , TestLabel "PESO positivo" $ TestCase $
        assertEqual "peso" (Just (2 :: Int))
            (reconocerCompleto lineaPeso "PESO: 2")

    , TestLabel "SALIDA sin requisito" $ TestCase $
        assertEqual "sin requisito"
            (Just (Salida Norte (IdSala "biblioteca") Nothing))
            (reconocerCompleto lineaSalida "SALIDA: norte -> biblioteca")

    , TestLabel "SALIDA con requisito" $ TestCase $
        assertEqual "con requisito"
            (Just (Salida Norte (IdSala "biblioteca") (Just (IdObjeto "llave"))))
            (reconocerCompleto lineaSalida "SALIDA: norte -> biblioteca requiere llave")

    , TestLabel "SALIDA direccion desconocida" $ TestCase $
        assertEqual "direccion invalida" (Nothing :: Maybe Salida)
            (reconocerCompleto lineaSalida "SALIDA: diagonal -> biblioteca")

    , TestLabel "Objeto con peso por defecto" $ TestCase $
        case reconocerCompleto leerBloqObj (unlines ["OBJETO: llave", "DESC: Una llave."]) of
            Just obj -> assertEqual "peso por defecto" (1 :: Int) (objetoPeso obj)
            Nothing -> assertFailure "No reconocio el bloque OBJETO"

    , TestLabel "Sala reconoce multiples salidas" $ TestCase $
        case reconocerCompleto leerBloqSala
                (unlines ["SALA: plaza", "DESC: Una plaza.", "SALIDA: norte -> biblioteca", "SALIDA: sur -> calle"]) of
            Just sala -> assertEqual "dos direcciones" (2 :: Int) (Map.size (salaSalidas sala))
            Nothing -> assertFailure "No reconocio el bloque SALA"

    , TestLabel "Cabecera admite omitir CAPACIDAD" $ TestCase $
        case reconocerCompleto leerCabecera
                (unlines ["MUNDO: Demo", "INICIO: entrada", "META: llegar a entrada"]) of
            Just m -> assertEqual "marca de capacidad pendiente"
                (0 :: Int) (mundoCapacidad m)
            Nothing -> assertFailure "No reconocio la cabecera sin CAPACIDAD"

    , TestLabel "Cabecera conserva CAPACIDAD explicita" $ TestCase $
        case reconocerCompleto leerCabecera
                (unlines ["MUNDO: Demo", "INICIO: entrada", "META: llegar a entrada", "CAPACIDAD: 7"]) of
            Just m -> assertEqual "capacidad explicita"
                (7 :: Int) (mundoCapacidad m)
            Nothing -> assertFailure "No reconocio la cabecera con CAPACIDAD"
    ]
