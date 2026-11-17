module Motor.Serializador
    ( serializarEstado
    , deserializarEstado
    ) where

import Motor.Tipos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Char (isSpace)
import Data.List (stripPrefix, isPrefixOf)

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
deserializarEstado mundo contenido = do
    -- Separar las secciones del archivo
    (titulo, idActual, idsInventario, lineasSalas, lineasSalidas, terminado) <-
        case lines contenido of
            (lMundo:lSala:lInventario:"objetos:":resto) -> do
                titulo <- campo "mundo: " lMundo
                idActual <- campo "sala: " lSala
                idsInventario <- campo "inventario: " lInventario

                let (salas, resto1) = break (== "desbloqueadas:") resto

                case resto1 of
                    [] -> Left "Falta la seccion desbloqueadas"
                    (_:resto2) -> do
                        let (salidas, resto3) =
                                break (isPrefixOf "terminado: ") resto2

                        case resto3 of
                            [lTerminado] -> do
                                valor <- campo "terminado: " lTerminado
                                finalizado <- case valor of
                                    "True"  -> Right True
                                    "False" -> Right False
                                    _       -> Left "Valor de terminado invalido"

                                Right (titulo, idActual, idsInventario,
                                       salas, salidas, finalizado)

                            _ -> Left "Formato de partida invalido"

            _ -> Left "Formato de partida invalido"

    -- Comprobar que corresponde al mismo mundo
    if titulo /= mundoTitulo mundo
        then Left "La partida pertenece a otro mundo"
        else Right ()

    -- Reconstruir los identificadores
    let sala = IdSala idActual

    inventarioLista <- mapM (Right . IdObjeto) =<< separarLista idsInventario

    salasGuardadas <- mapM leerSala lineasSalas
    salidasGuardadas <- mapM leerSalida lineasSalidas

    let inventarioNuevo = Set.fromList inventarioLista

        objetosNuevos = Map.fromList
            [ (idSala, Set.fromList objetos) | (idSala, objetos) <- salasGuardadas ]

        salidasNuevas = Set.fromList salidasGuardadas

        idsSalas = Set.fromList (Map.keys (mundoSalas mundo))
        idsObjetos = Set.fromList (Map.keys (mundoObjetos mundo))

        objetosEnSalas = concatMap snd salasGuardadas

        todosLosObjetos = inventarioLista ++ objetosEnSalas

        salidasValidas = all existeSalida salidasGuardadas

    -- Validaciones
    if Set.notMember sala idsSalas
        then Left "La sala actual no existe"
    else if length salasGuardadas /= Map.size objetosNuevos
        then Left "Hay salas repetidas"
    else if any (`Set.notMember` idsSalas) (Map.keys objetosNuevos)
        then Left "La partida contiene salas inexistentes"
    else if Set.fromList (Map.keys objetosNuevos) /= idsSalas
        then Left "Las salas guardadas no coinciden con las del mundo"
    else if any (`Set.notMember` idsObjetos) todosLosObjetos
        then Left "La partida contiene objetos inexistentes"
    else if length todosLosObjetos /= Set.size (Set.fromList todosLosObjetos)
        then Left "Hay objetos duplicados en la partida"
    else if Set.fromList todosLosObjetos /= idsObjetos
        then Left "Faltan objetos en la partida"
    else if not salidasValidas
        then Left "La partida contiene salidas inexistentes"
    else if length salidasGuardadas /= Set.size salidasNuevas
        then Left "Hay salidas desbloqueadas repetidas"
    else do
        pesos <- mapM pesoObjeto inventarioLista

        if sum pesos > mundoCapacidad mundo
            then Left "El inventario supera la capacidad"
            else Right Estado
                { estadoMundo = mundo
                , salaActual = sala
                , inventario = inventarioNuevo
                , objetosPorSala = objetosNuevos
                , salidasDesbloqueadas = salidasNuevas
                , juegoTerminado = terminado
                }

    where
        limpiar = reverse . dropWhile isSpace . reverse . dropWhile isSpace

        campo prefijo linea =
            case stripPrefix prefijo linea of
                Just valor -> Right valor
                Nothing    -> Left ("Falta el campo " ++ prefijo)

        separarLista "" = Right []
        separarLista texto =
            let elementos = map limpiar (separar texto)
            in if any null elementos
                then Left "Lista de objetos mal formada"
                else Right elementos

        separar texto =
            case break (== ',') texto of
                (x, [])     -> [x]
                (x, _:resto) -> x : separar resto

        leerPar linea =
            case break (== ':') linea of
                (identificador, ':':valor)
                    | not (null (limpiar identificador)) ->
                        Right (limpiar identificador, limpiar valor)
                _ -> Left ("Linea mal formada: " ++ linea)

        leerSala linea = do
            (identificador, objetos) <- leerPar linea
            lista <- separarLista objetos
            Right (IdSala identificador, map IdObjeto lista)

        leerSalida linea = do
            (identificador, direccion) <- leerPar linea
            dir <- case lookup direccion direcciones of
                Just d  -> Right d
                Nothing -> Left ("Direccion invalida: " ++ direccion)
            Right (IdSala identificador, dir)

        direcciones =
            [ ("Norte", Norte)
            , ("Sur", Sur)
            , ("Este", Este)
            , ("Oeste", Oeste)
            , ("Arriba", Arriba)
            , ("Abajo", Abajo)
            ]

        existeSalida (idSala, direccion) =
            case Map.lookup idSala (mundoSalas mundo) of
                Just (s:_) -> Map.member direccion (salaSalidas s)
                _          -> False

        pesoObjeto identificador =
            case Map.lookup identificador (mundoObjetos mundo) of
                Just (obj:_) -> Right (objetoPeso obj)
                _ -> Left "No se pudo obtener el peso de un objeto"
