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

ejecutar (Ir dir) estado =
    case salida of
        []  -> (["No hay salida en dirección " ++ show dir], estado)
        _   -> ([""], estado)
    where
        salas = mundoSalas (estadoMundo estado)
        -- !! no falla ya que la sala actual debe estar en la lista de salas. Esta comprobación
        -- se hace en Motor.Mundo
        sala = (Map.findWithDefault [] (salaActual estado) salas) !! 0
        salida = Map.findWithDefault [] dir (salaSalidas sala)

ejecutar Mirar estado =
    (   [ "Sala actual: " ++ show (salaActual estado)
        , desc
        , ""
        , "Objetos:"
        ]
        ++ case objs of
            []  -> ["(Sin objetos)"]
            _   -> objs
        ++ [ ""
        , "Salidas:"
        ]
        ++ case salidas of
            []  -> ["(Sin salidas)"]
            _   -> salidas
    , estado
    )
    where
        salas = mundoSalas (estadoMundo estado)
        -- !! no falla ya que la sala actual debe estar en la lista de salas. Esta comprobación
        -- se hace en Motor.Mundo
        sala = (Map.findWithDefault [] (salaActual estado) salas) !! 0
        desc = salaDesc sala
        objs = map (\(IdObjeto objId) -> objId) (salaObjetos sala)
        salidas = map (\s -> show s) (concat (Map.elems (salaSalidas sala)))


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

ejecutar Ayuda estado =
    ("Comandos disponibles:" : map ayudaComando comandos, estado)
    where
        comandos =
            [ Ir Norte
            , Mirar
            , Tomar (IdObjeto "")
            , Soltar (IdObjeto "")
            , Examinar (IdObjeto "")
            , Inventario
            , Guardar ""
            , Cargar ""
            , Ayuda
            , Salir
            ]

ejecutar Salir estado =
    ( [ "Saliendo del juego..." ]
    , estado { juegoTerminado = True }
    )

-- Función Auxiliar: ayudaComando
ayudaComando :: Comando -> String
ayudaComando (Ir _)         = "ir <dirección>: Moverse en una dirección "
                                ++"(norte, sur, este, oeste, arriba, abajo)"
ayudaComando Mirar          = "mirar: describir la sala actual"
ayudaComando (Tomar _)      = "tomar <objeto>: Recoger un objeto"
ayudaComando (Soltar _)     = "soltar <objeto>: Dejar un objeto"
ayudaComando (Examinar _)   = "examinar <objeto>: Lee la descripción de un objeto del inventario o la sala"
ayudaComando Inventario     = "inventario: Mostrar los objetos en el inventario"
ayudaComando (Guardar _)    = "guardar <archivo>: Guardar la partida en el archivo indicado"
ayudaComando (Cargar _)     = "cargar <archivo>: Cargar una partida del archivo indicado"
ayudaComando Ayuda          = "ayuda: Mostrar los comandos disponibles"
ayudaComando Salir          = "salidar: Terminar el juego"