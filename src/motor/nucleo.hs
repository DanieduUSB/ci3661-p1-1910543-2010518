module Motor.Nucleo
    ( estadoInicial
    , ejecutar
    ) where

import Motor.Tipos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Recibe el Mundo 'm' ya validado y arma el estado inicial del juego con este
estadoInicial :: Mundo -> Estado
estadoInicial m = Estado
    { estadoMundo       = m
    , salaActual        = mundoInicio m
    , inventario        = Set.empty
    , objetosPorSala    = Map.fromList
        [ (salaId s, Set.fromList (salaObjetos s))
        | grupo <- Map.elems (mundoSalas m)
        , s <- grupo
        ]
    , juegoTerminado    = False
    }

-- Para cada comando existente realiza una acción distinta
ejecutar :: Comando -> Estado -> ([String], Estado)

ejecutar (Ir dir) estado = undefined

ejecutar Mirar estado = undefined

ejecutar (Tomar idObj) estado = undefined

ejecutar (Soltar idObj) estado = undefined

ejecutar (Examinar idObj) estado = undefined

ejecutar Inventario estado = undefined

ejecutar (Guardar path) estado = undefined

ejecutar (Cargar path) estado = undefined

ejecutar Ayuda estado = undefined

ejecutar Salir estado = undefined