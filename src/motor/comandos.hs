module Motor.Comandos
    ( leerComando
    ) where

import Motor.Tipos

leerComando :: String -> Maybe Comando
leerComando entrada =
    case words (map toLower entrada) of
        ["mirar"] -> Just Mirar
        ["inventario"] -> Inventario
        ["ayuda"] -> Ayuda
        ["salir"] -> Salir
