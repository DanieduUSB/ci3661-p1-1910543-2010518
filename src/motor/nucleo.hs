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

ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar = undefined