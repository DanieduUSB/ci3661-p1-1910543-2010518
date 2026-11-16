module Motor.Serializador
    ( serializarEstado
    , deserializarEstado
    ) where

import Motor.Tipos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate)

serializarEstado :: Estado -> String
serializarEstado estado =
    unlines
        [ "mundo: " ++ mundoTitulo (estadoMundo estado)
        , "sala: " ++ obtenerIdSala (salaActual estado)
        , "inventario: " ++ intercalate ","
            (map obtenerIdObjeto (Set.toList (inventario estado)))
        , "objetos:"
        ] ++
    unlines
        [ obtenerIdSala sala ++ ": " ++
          intercalate "," (map obtenerIdObjeto (Set.toList objetos))
        | (sala, objetos) <- Map.toList (objetosPorSala estado)
        ] ++
    unlines
        [ "desbloqueadas:"
        ] ++
    unlines
        [ obtenerIdSala sala ++ ": " ++ show direccion
        | (sala, direccion) <- Set.toList (salidasDesbloqueadas estado)
        ] ++
    "terminado: " ++ show (juegoTerminado estado) ++ "\n"
    where
        obtenerIdSala (IdSala identificador)        = identificador
        obtenerIdObjeto (IdObjeto identificador)    = identificador

deserializarEstado :: Mundo -> String -> Either String Estado
deserializarEstado = undefined