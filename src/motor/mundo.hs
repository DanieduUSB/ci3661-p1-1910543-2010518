module Motor.Mundo
    ( leerMundo
    , prepararLineas
    , separarBloques
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

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo = undefined

reconocerSintaxis :: String -> Either [ErrorMundo] Mundo
reconocerSintaxis = undefined

numeroBloque :: Bloque -> Int
numeroBloque ((numero, _):_) = numero
numeroBloque [] = 1

textoBloque :: Bloque -> String
textoBloque bloque =
    unlines (map snd bloque)

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

agruparPor :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
agruparPor obtenerClave =
    foldr agregar Map.empty
    where
        agregar valor mapa =
            Map.insertWith (++)
                (obtenerClave valor)
                [valor]
                mapa

data TipoBloque = TCabecera | TObjeto | TSala
    deriving (Eq, Show)

-- (Nombre del campo, obligatorio, puede repetirse)
reglas :: TipoBloque -> [(String, Bool, Bool)]
reglas TCabecera =
    [ ("MUNDO", True, False)
    , ("INICIO", True, False)
    , ("META", True, False)
    , ("CAPACIDAD", True, False)
    ]

reglas TObjeto =
    [ ("OBJETO", True, False)
    , ("DESC", True, False)
    , ("PESO", False, False)
    ]

reglas TSala =
    [ ("SALA", True, False)
    , ("DESC", True, False)
    , ("SALIDA", False, True)
    , ("OBJETO", False, True)
    ]

partirClave :: String -> Maybe (String, String)
partirClave linea =
    case break (== ':') linea of
        (nombre, ':' : valor)
            | not (null nombre)
            , all (\c -> c >= 'A' && c <= 'Z') nombre
                -> Just (nombre, valor)

        _ -> Nothing

validarValor :: LineaNumerada -> String -> String -> [ErrorMundo]
validarValor (numero, linea) nombre valor =
    case nombre of
        "MUNDO" ->
            comprobar lineaMundo (FormaClaveValor linea)

        "INICIO" ->
            comprobar lineaInicio (IdInvalido valor)

        "META" ->
            comprobar lineaMeta (MetaMalFormada linea)

        "CAPACIDAD" ->
            comprobar lineaCapacidad (FormaClaveValor linea)

        "OBJETO" ->
            comprobar lineaObjeto (IdInvalido valor)

        "SALA" ->
            comprobar lineaSala (IdInvalido valor)

        "DESC" ->
            comprobar lineaDesc (FormaClaveValor linea)

        "SALIDA" ->
            comprobar lineaSalida (errorSalida linea valor)

        "PESO" ->
            comprobar lineaPeso (FormaClaveValor linea)

        _ ->
            [V1 numero (ClaveDesconocida nombre)]

    where
        comprobar :: ReadS a -> ErrorV1 -> [ErrorMundo]
        comprobar reconocedor errorV =
            case reconocerCompleto reconocedor linea of
                Just _  -> []
                Nothing -> [V1 numero errorV]

errorSalida :: String -> String -> ErrorV1
errorSalida linea valor =
    case words valor of
        (dir:_)
            | dir `notElem`
                ["norte", "sur", "este", "oeste", "arriba", "abajo"]
                -> DireccionInexistente dir

        _ -> SalidaMalFormada linea