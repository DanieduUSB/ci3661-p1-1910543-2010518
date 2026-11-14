module Motor.Reconocedor
    ( leerMun
    , leerInicio
    , leerMeta
    , leerObjeto
    , leerDesc
    , leerSalida
    , leerObjSala
) where

import Data.List(stripPrefix)
import Motor.Tipos

leerMun :: String -> Either String String
leerMun m =
    case stripPrefix "MUNDO: " m of
        Nothing -> Left "Formato inválido"
        Just titulo -> Right titulo
        
leerInicio :: String -> Either String String
leerInicio = undefined

leerMeta :: String -> Either String String
leerMeta = undefined

leerObjeto :: String -> Either String String
leerObjeto = undefined

leerDesc :: String -> Either String String
leerDesc = undefined

leerSalida:: String -> Either String String
leerSalida = undefined

leerObjSala :: String -> Either String String
leerObjSala = undefined