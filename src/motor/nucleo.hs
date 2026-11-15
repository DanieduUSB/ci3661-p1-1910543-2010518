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

ejecutar (Examinar idObj) estado
    -- Caso: El objeto no está en el inventario ni en la sala
    | not (Set.member idObj (inventario estado)) &&
      not (Set.member idObj objetosSala)
        = (["No tienes acceso al objeto " ++ show idObj], estado)
    | otherwise
        = (strLista, estado)
    where
        objetosSala = Map.findWithDefault Set.empty
            (salaActual estado)
            (objetosPorSala estado)
        -- Lista global de objetos
        objs        = mundoObjetos (estadoMundo estado)
        strLista    = case Map.lookup idObj objs of
            -- Caso: El objeto existe
            Just [obj]  -> [show idObj ++ ":", objetoDesc obj]
            -- Caso: El objeto no existe (No fue declarado nunca)
            _           -> ["No existe el objeto " ++ show idObj]

ejecutar Inventario estado =
    ( "Inventario:" : contenido, estado )
    where
        inv = inventario estado
        contenido
            | Set.null inv  = ["(Vacío)"]
            | otherwise     = map (\(IdObjeto str) -> str) (Set.toList inv)

ejecutar (Guardar path) estado = undefined

ejecutar (Cargar path) estado = undefined

ejecutar Ayuda estado = undefined

ejecutar Salir estado =
    ( [ "Saliendo del juego..." ]
    , estado { juegoTerminado = True }
    )