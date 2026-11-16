module Motor.Mundo
    ( leerMundo
    , prepararLineas
    , separarBloques
    , esBloque
    , organizarBloques
    ) where

import Motor.Tipos ( ErrorMundo, Mundo )

import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)

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