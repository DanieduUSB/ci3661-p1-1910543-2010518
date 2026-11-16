module Datos.EstadoPrueba
    ( estadoPruebaConRequisito
    ) where

import Motor.Tipos
import Motor.Nucleo (estadoInicial)
import Datos.MundoPrueba

-- Estado inicial: jugador en la calle, sin objetos
estadoPruebaConRequisito :: Estado
estadoPruebaConRequisito = estadoInicial mundoMetaConRequisito
