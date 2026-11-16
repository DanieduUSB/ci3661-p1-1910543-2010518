module Motor.Serializador
    ( serializarEstado
    , deserializarEstado
    ) where

import Motor.Tipos

serializarEstado :: Estado -> String
serializarEstado = undefined

deserializarEstado :: Mundo -> String -> Either String Estado
deserializarEstado = undefined