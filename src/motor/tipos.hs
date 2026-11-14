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

-- Identificador

newtype Id = Id String
    deriving (Eq, Ord, Show)

-- Mundo

data Mundo = Mundo

-- Sala

data Sala = Sala

-- Objeto

data Objeto = Objeto
    { id    :: Id
    , desc  :: String
    , peso  :: Int
    } deriving (Eq, Show)

-- Salida

data Salida = Salida
    { direccion :: Direccion
    , destino   :: Id
    , requisito :: Maybe Id
    } deriving (Eq, Ord, Show)

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