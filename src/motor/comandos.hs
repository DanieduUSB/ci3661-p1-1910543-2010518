module Motor.Comandos
    ( leerComando
    ) where

import Motor.Tipos
import Data.Char (toLower)

leerComando :: String -> Maybe Comando
leerComando entrada =
    case words (map toLower entrada) of
        ["mirar"]           -> Just Mirar
        ["tomar", idObj]    -> Just (Tomar (IdObjeto idObj))
        ["coger", idObj]    -> Just (Tomar (IdObjeto idObj))
        ["soltar", obj]     -> Just (Soltar (IdObjeto idObj))
        ["examinar", obj]   -> Just (Examinar (IdObjeto idObj))
        ["inventario"]      -> Just Inventario
        ["inv"]             -> Just Inventario
        ["guardar", path]   -> Just (Guardar path)
        ["cargar", path]    -> Just (Cargar path)
        ["ayuda"]           -> Just Ayuda
        ["salir"]           -> Just Salir
        _                   -> Nothing
