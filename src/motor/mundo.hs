module Motor.Mundo
    ( leerMundo
    ) where

import Motor.Tipos ( ErrorMundo, Mundo )

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo = undefined