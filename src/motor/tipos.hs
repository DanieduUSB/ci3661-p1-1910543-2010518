module Motor.Tipos
    ( Mundo(..)
    , Sala (..)
    , Objeto(..)
    , Direccion(..)
    , Meta(..)
    , Estado(..)
    , Comando(..)
    , ErrorMundo(..)
    ) where

data Mundo = Mundo

data Sala = Sala

data Objeto = Objeto

-- Direcciones

data Direccion
    = Norte
    | Sur
    | Este
    | Oeste
    | Arriba
    | Abajo
    deriving (Eq, Ord, Show)

data Meta = Meta

data Estado = Estado

data Comando = Comando

data ErrorMundo = ErrorMundo