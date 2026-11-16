module Motor.Mundo
    ( leerMundo
    , prepararLineas
    , separarBloques
    , esBloque
    , organizarBloques
    , reconocerSintaxis
    ) where

import Motor.Tipos
import Motor.Reconocedor

import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map

-- Tipos de dato auxiliares
type LineaNumerada = (Int, String)

type Bloque = [LineaNumerada]

data BloquesTexto = BloquesTexto
    { bloqueCabecera     :: Bloque
    , bloquesObjetos     :: [Bloque]
    , bloquesSalas       :: [Bloque]
    , bloquesDesconocidos :: [Bloque]
    } deriving (Show)

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo = undefined

reconocerSintaxis :: String -> Either [ErrorMundo] Mundo
reconocerSintaxis texto =
    let bloques = organizarBloques texto

        -- Reconocer la cabecera
        (erroresCab, cabeceras) =
            partitionEithers
                [reconocerBloque "cabecera" leerCabecera
                    (bloqueCabecera bloques)]

        -- Reconocer todos los objetos
        (erroresObj, objetos) =
            partitionEithers
                [ reconocerBloque "objeto" leerBloqObj bloque
                | bloque <- bloquesObjetos bloques
                ]

        -- Reconocer todas las salas
        (erroresSala, salas) =
            partitionEithers
                [ reconocerBloque "sala" leerBloqSala bloque
                | bloque <- bloquesSalas bloques
                ]

        -- Registrar bloques de tipo desconocido
        erroresOtros =
            concatMap errorBloqueDesconocido
                (bloquesDesconocidos bloques)

        -- Reunir errores
        errores =
            erroresCab ++ erroresObj ++ erroresSala ++ erroresOtros

    in case (errores, cabeceras) of
        ([], [cabecera]) ->
            Right cabecera
                { mundoSalas = agruparPor salaId salas
                , mundoObjetos = agruparPor objetoId objetos
                }

        (_:_, _) -> Left errores

        ([], _) -> Left [V1 1 (CampoAusente "MUNDO")]

reconocerBloque :: String -> ReadS a -> Bloque -> Either ErrorMundo a
reconocerBloque = undefined

numeroBloque :: Bloque -> Int
numeroBloque ((numero, _):_) = numero
numeroBloque [] = 1

textoBloque :: Bloque -> String
textoBloque bloque =
    unlines (map snd bloque)

organizarBloques :: String -> BloquesTexto
organizarBloques texto =
    case separarBloques texto of
        [] -> BloquesTexto [] [] [] []

        (cabecera:resto) ->
            let (objetos, salas, desconocidos) =
                    foldr clasificar ([], [], []) resto
            in BloquesTexto cabecera objetos salas desconocidos

    where
        clasificar bloque (objetos, salas, desconocidos)
            | esBloque "OBJETO:" bloque =
                (bloque:objetos, salas, desconocidos)

            | esBloque "SALA:" bloque =
                (objetos, bloque:salas, desconocidos)

            | otherwise =
                (objetos, salas, bloque:desconocidos)

prepararLineas :: String -> [LineaNumerada]
prepararLineas texto =
    [ (numero, linea)
    | (numero, original) <- zip [1..] (lines texto)
    , let sinComentario = takeWhile (/= '#') original
    , let linea = limpiar sinComentario
    , not (null linea)
    ]
    where
        limpiar = dropWhileEnd isSpace . dropWhile isSpace

separarBloques :: String -> [Bloque]
separarBloques texto =
    foldr separar [[]] (prepararLineas texto)
    where
        separar (_, "--") bloques = [] : bloques

        separar linea (bloque:resto) =
            (linea:bloque) : resto

        separar linea [] = [[linea]]

esBloque :: String -> Bloque -> Bool
esBloque clave ((_, linea):_) =
    clave `isPrefixOf` linea

esBloque _ [] = False

-- Agrupa por clave
agruparPor :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
agruparPor obtenerClave =
    foldr agregar Map.empty
    where
        agregar valor mapa =
            Map.insertWith (++)
                (obtenerClave valor)
                [valor]
                mapa

errorBloqueDesconocido :: Bloque -> [ErrorMundo]
errorBloqueDesconocido = undefined