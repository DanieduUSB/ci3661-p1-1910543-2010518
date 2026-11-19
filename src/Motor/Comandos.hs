module Motor.Comandos
    ( leerComando
    ) where

import Motor.Tipos
import Data.Char (toLower)

leerComando :: String -> Maybe Comando
leerComando entrada =
    case words (map toLower entrada) of
        ["ir", dir]         -> Ir <$> leerDirecion dir
        ["mirar"]           -> Just Mirar
        ["tomar", idObj]    -> Just (Tomar (IdObjeto idObj))
        ["coger", idObj]    -> Just (Tomar (IdObjeto idObj))
        ["soltar", idObj]   -> Just (Soltar (IdObjeto idObj))
        ["examinar", idObj] -> Just (Examinar (IdObjeto idObj))
        ["inventario"]      -> Just Inventario
        ["inv"]             -> Just Inventario
        ["guardar", path]   -> Just (Guardar path)
        ["cargar", path]    -> Just (Cargar path)
        ["ayuda"]           -> Just Ayuda
        ["salir"]           -> Just Salir
        _                   -> Nothing


-- Auxiliar leerDirecion: Lee un string y ve si corresponde a una Direccion
leerDirecion :: String -> Maybe Direccion
leerDirecion "norte"    = Just Norte
leerDirecion "sur"      = Just Sur
leerDirecion "este"     = Just Este
leerDirecion "oeste"    = Just Oeste
leerDirecion "arriba"   = Just Arriba
leerDirecion "abajo"    = Just Abajo
leerDirecion _          = Just Invalida