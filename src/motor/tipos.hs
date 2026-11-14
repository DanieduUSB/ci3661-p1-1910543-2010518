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

-- Mundo

data Mundo = Mundo

-- Sala

data Sala = Sala

-- Objeto

data Objeto = Objeto
    { id :: String
    , desc :: String
    , peso :: Int
    } deriving (Eq, Show)

-- Direcciones

data Direccion
    = Norte
    | Sur
    | Este
    | Oeste
    | Arriba
    | Abajo
    deriving (Eq, Ord, Show)

-- Meta

data Meta = Meta

-- Estado

data Estado = Estado

-- Comando

data Comando = Comando

-- ErrorMundo

data ErrorMundo = ErrorMundo