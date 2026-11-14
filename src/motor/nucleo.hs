module Motor.Nucleo
    ( estadoInicial
    , ejecutar
    ) where

import Motor.Tipos ( Mundo, Estado, Comando )

estadoInicial :: Mundo -> Estado
estadoInicial = undefined

ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar = undefined