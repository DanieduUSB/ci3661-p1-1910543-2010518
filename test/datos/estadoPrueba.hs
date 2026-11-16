module Datos.EstadoPrueba
    ( estadoPrueba
    , estadoConLlaveMagnetica
    ) where

import qualified Data.Set as Set

import Motor.Tipos
import Motor.Nucleo (estadoInicial)
import Datos.MundoPrueba

-- Estado inicial: jugador en la calle, sin objetos
estadoPrueba :: Estado
estadoPrueba = estadoInicial mundoMetaConRequisito


-- Jugador con la llave magnética
estadoConLlaveMagnetica :: Estado
estadoConLlaveMagnetica = estadoPrueba
    { inventario = Set.singleton (IdObjeto "llave_magnetica")
    }