module Motor.Mundo
    ( leerMundo
    , prepararLineas
    , separarBloques
    ) where

import Motor.Tipos ( ErrorMundo, Mundo )

import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)

-- Tipos de dato auxiliares
type LineaNumerada = (Int, String)

type Bloque = [LineaNumerada]

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo = undefined

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