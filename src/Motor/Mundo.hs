module Motor.Mundo
    ( leerMundo
    , prepararLineas
    , separarBloques
    ) where

import Motor.Tipos
import Motor.Reconocedor

import Data.Char (isSpace)
import Data.List (dropWhileEnd, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Tipos de dato auxiliares
type LineaNumerada = (Int, String)
type Bloque = [LineaNumerada]
data TipoBloque = TCabecera | TObjeto | TSala
    deriving (Eq, Show)

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo texto =
    case separarBloques texto of

        [] ->
            Left [V1 1 (CampoAusente "MUNDO")]

        (cabecera:resto) ->
            let erroresCabecera =
                    validarBloque TCabecera cabecera

                (erroresOtros, salas, objetos) =
                    foldr procesarBloque ([], [], []) resto

                errores =
                    sortOn numeroError
                        ( erroresCabecera
                        ++ erroresOtros
                        ++ erroresSeparadores texto
                        )

            in if not (null errores)
                then Left errores
                else
                    case reconocerCompleto leerCabecera (textoBloque cabecera) of

                        Nothing ->
                           Left [V1 (numeroBloque cabecera)
                                    (FormaClaveValor "Cabecera no reconocida")]

                        Just mundoCabecera ->
                            let mundoSinCapacidad = mundoCabecera
                                    { mundoSalas = agruparPor salaId salas
                                    , mundoObjetos = agruparPor objetoId objetos
                                    }

                                -- Si CAPACIDAD no aparece, leerCabecera deja 0
                                -- como marca temporal. Aquí ya conocemos todos
                                -- los objetos y podemos calcular la capacidad.
                                capacidadFinal =
                                    if mundoCapacidad mundoSinCapacidad == 0
                                        then max 1 (capacidadRequerida mundoSinCapacidad)
                                        else mundoCapacidad mundoSinCapacidad

                                mundo = mundoSinCapacidad
                                    { mundoCapacidad = capacidadFinal }

                                erroresSemanticos =
                                    validarV2 mundo
                                    ++ validarV3 mundo
                                    ++ validarV4 mundo
                                    ++ validarV5 mundo
                                    ++ validarV6 mundo
                                    ++ validarV7 mundo
                                    ++ validarV8 mundo
                                    ++ validarV9 mundo
                                    ++ validarV10 mundo
                                    ++ validarPesoIndividual mundo
                                    ++ validarCapacidad mundo

                            in case erroresSemanticos of
                                [] -> Right mundo
                                _  -> Left erroresSemanticos

    where
        numeroError (V1 numero _) = numero
        numeroError _ = maxBound

-- Función recibe un bloque y lo procesa
procesarBloque
    :: Bloque
    -> ([ErrorMundo], [Sala], [Objeto])
    -> ([ErrorMundo], [Sala], [Objeto])

procesarBloque [] acumulador = acumulador

procesarBloque bloque (errores, salas, objetos) =
    case tipoBloque bloque of

        Just TSala ->
            let erroresBloque = validarBloque TSala bloque
            in if not (null erroresBloque)
               then (erroresBloque ++ errores, salas, objetos)
               else
                   case reconocerCompleto leerBloqSala (textoBloque bloque) of
                       Just sala ->
                           (errores, sala:salas, objetos)

                       Nothing ->
                           (errorReconocimiento "SALA" : errores, salas, objetos)

        Just TObjeto ->
            let erroresBloque = validarBloque TObjeto bloque
            in if not (null erroresBloque)
               then (erroresBloque ++ errores, salas, objetos)
               else
                   case reconocerCompleto leerBloqObj (textoBloque bloque) of
                       Just objeto ->
                           (errores, salas, objeto:objetos)

                       Nothing ->
                           (errorReconocimiento "OBJETO" : errores, salas, objetos)

        _ ->
            (validarDesconocido bloque ++ errores, salas, objetos)

    where
        errorReconocimiento nombre =
            V1 (numeroBloque bloque)
               (FormaClaveValor ("Bloque " ++ nombre ++ " no reconocido"))

validarV2 :: Mundo -> [ErrorMundo]
validarV2 mundo =
    erroresSalas ++ erroresObjetos
    where
        erroresSalas =
            [ V2a identificador
            | (identificador, salas) <- Map.toList (mundoSalas mundo)
            , length salas > 1
            ]

        erroresObjetos =
            [ V2b identificador
            | (identificador, objetos) <- Map.toList (mundoObjetos mundo)
            , length objetos > 1
            ]

validarV3 :: Mundo -> [ErrorMundo]
validarV3 mundo =
    [ V3 (salaId sala) (salidaDestino salida)
    | definiciones <- Map.elems (mundoSalas mundo)
    , sala <- definiciones
    , salidas <- Map.elems (salaSalidas sala)
    , salida <- salidas
    , Map.notMember (salidaDestino salida) (mundoSalas mundo)
    ]

validarV4 :: Mundo -> [ErrorMundo]
validarV4 mundo =
    [ V4 (salaId sala) objeto
    | definiciones <- Map.elems (mundoSalas mundo)
    , sala <- definiciones
    , salidas <- Map.elems (salaSalidas sala)
    , salida <- salidas
    , Just objeto <- [salidaRequisito salida]
    , Map.notMember objeto (mundoObjetos mundo)
    ]

validarV5 :: Mundo -> [ErrorMundo]
validarV5 mundo =
    [ V5 (salaId sala) direccion
    | definiciones <- Map.elems (mundoSalas mundo)
    , sala <- definiciones
    , (direccion, salidas) <- Map.toList (salaSalidas sala)
    , length salidas > 1
    ]

validarV6 :: Mundo -> [ErrorMundo]
validarV6 mundo =
    [ V6 (salaId sala) objeto
    | definiciones <- Map.elems (mundoSalas mundo)
    , sala <- definiciones
    , objeto <- salaObjetos sala
    , Map.notMember objeto (mundoObjetos mundo)
    ]

validarV7 :: Mundo -> [ErrorMundo]
validarV7 mundo =
    [ V7 objeto
    | (objeto, cantidad) <- Map.toList apariciones
    , cantidad > 1
    ]
    where
        apariciones =
            Map.fromListWith (+)
                [ (objeto, 1 :: Int)
                | definiciones <- Map.elems (mundoSalas mundo)
                , sala <- definiciones
                , objeto <- salaObjetos sala
                ]

validarV8 :: Mundo -> [ErrorMundo]
validarV8 mundo =
    erroresInicio ++ erroresMeta
    where
        salas = mundoSalas mundo
        objetos = mundoObjetos mundo

        inicio = mundoInicio mundo

        erroresInicio =
            [ V8a inicio
            | Map.notMember inicio salas
            ]

        erroresMeta =
            case mundoMeta mundo of

                Llegar destino ->
                    [ V8b destino
                    | Map.notMember destino salas
                    ]

                Llevar objeto destino ->
                    [ V8b destino
                    | Map.notMember destino salas
                    ]
                    ++
                    [ V8c objeto
                    | Map.notMember objeto objetos
                    ]

validarV9 :: Mundo -> [ErrorMundo]
validarV9 mundo
    | Map.notMember (mundoInicio mundo) (mundoSalas mundo) = []
    | otherwise =
        [ V9 sala
        | sala <- Map.keys (mundoSalas mundo)
        , Set.notMember sala (salasAlcanzables mundo)
        ]

validarV10 :: Mundo -> [ErrorMundo]
validarV10 mundo =
    [ V10 objeto
    | objeto <- Set.toList (objetosNecesarios mundo)
    , Map.member objeto (mundoObjetos mundo)
    , Set.notMember objeto objetosColocados
    ]
    where
        objetosColocados =
            Set.fromList
                [ objeto
                | definiciones <- Map.elems (mundoSalas mundo)
                , sala <- definiciones
                , objeto <- salaObjetos sala
                ]

validarPesoIndividual :: Mundo -> [ErrorMundo]
validarPesoIndividual mundo =
    [ PesoExcesivo (objetoId objeto) (objetoPeso objeto) capacidad
    | definiciones <- Map.elems (mundoObjetos mundo)
    , objeto <- definiciones
    , objetoPeso objeto > capacidad
    ]
    where
        capacidad = mundoCapacidad mundo

validarCapacidad :: Mundo -> [ErrorMundo]
validarCapacidad mundo
    | not (null (validarPesoIndividual mundo)) = []
    | hayObjetosInexistentes = []
    | capacidad < requerida =
        [CapacidadInsuficiente capacidad requerida]
    | otherwise = []
    where
        capacidad = mundoCapacidad mundo
        requerida = capacidadRequerida mundo

        hayObjetosInexistentes =
            any
                (\objeto -> Map.notMember objeto (mundoObjetos mundo))
                (Set.toList (objetosNecesarios mundo))

numeroBloque :: Bloque -> Int
numeroBloque ((numero, _):_) = numero
numeroBloque [] = 1

textoBloque :: Bloque -> String
textoBloque bloque =
    unlines (map snd bloque)

prepararLineas :: String -> [LineaNumerada]
prepararLineas texto =
    [ (numero, linea)
    | (numero, original) <- zip [1..] (lines texto)
    , let sinComentario = takeWhile (/= '#') original
    , let linea = limpiar sinComentario
    , not (null linea)
    ]
    where
        limpiar = dropWhileEnd isSpace . dropWhile isSpace

separarBloques :: String -> [Bloque]
separarBloques texto =
    foldr separar [[]] (prepararLineas texto)
    where
        separar (_, "--") bloques = [] : bloques

        separar linea (bloque:resto) =
            (linea:bloque) : resto

        separar linea [] = [[linea]]

agruparPor :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
agruparPor obtenerClave =
    foldr agregar Map.empty
    where
        agregar valor mapa =
            Map.insertWith (++)
                (obtenerClave valor)
                [valor]
                mapa

-- (Nombre del campo, obligatorio, puede repetirse)
reglas :: TipoBloque -> [(String, Bool, Bool)]
reglas TCabecera =
    [ ("MUNDO", True, False)
    , ("INICIO", True, False)
    , ("META", True, False)
    , ("CAPACIDAD", False, False)
    ]

reglas TObjeto =
    [ ("OBJETO", True, False)
    , ("DESC", True, False)
    , ("PESO", False, False)
    ]

reglas TSala =
    [ ("SALA", True, False)
    , ("DESC", True, False)
    , ("SALIDA", False, True)
    , ("OBJETO", False, True)
    ]

partirClave :: String -> Maybe (String, String)
partirClave linea =
    case break (== ':') linea of
        (nombre, ':' : valor)
            | not (null nombre)
            , all (\c -> c >= 'A' && c <= 'Z') nombre
                -> Just (nombre, valor)

        _ -> Nothing

-- Función para validar la sintaxis de un bloque conocido
validarBloque :: TipoBloque -> Bloque -> [ErrorMundo]
validarBloque tipo bloque =
    concatMap (validarLinea tipo) bloque
    ++ validarCantidad tipo bloque

validarLinea :: TipoBloque -> LineaNumerada -> [ErrorMundo]
validarLinea tipo lineaNumerada@(numero, linea) =
    case partirClave linea of
        Nothing ->
            [V1 numero (FormaClaveValor linea)]

        Just (nombre, valor)
            | nombre `elem` permitidas ->
                validarValor lineaNumerada nombre valor

            | otherwise ->
                [V1 numero (ClaveDesconocida nombre)]

    where
        permitidas = [nombre | (nombre, _, _) <- reglas tipo]

validarCantidad :: TipoBloque -> Bloque -> [ErrorMundo]
validarCantidad tipo bloque =
    concatMap revisar (reglas tipo)
    where
        revisar (nombre, obligatorio, repetible) =
            let apariciones =
                    [ numero
                    | (numero, linea) <- bloque
                    , Just (clave, _) <- [partirClave linea]
                    , clave == nombre
                    ]

                ausentes =
                    [ V1 (numeroBloque bloque) (CampoAusente nombre)
                    | obligatorio && null apariciones
                    ]

                repetidos =
                    [ V1 numero (CampoRepetido nombre)
                    | not repetible
                    , numero <- drop 1 apariciones
                    ]

            in ausentes ++ repetidos

validarValor :: LineaNumerada -> String -> String -> [ErrorMundo]
validarValor (numero, linea) nombre valor =
    case nombre of
        "MUNDO" ->
            comprobar lineaMundo (FormaClaveValor linea)

        "INICIO" ->
            comprobar lineaInicio (IdInvalido valor)

        "META" ->
            comprobar lineaMeta (MetaMalFormada linea)

        "CAPACIDAD" ->
            comprobar lineaCapacidad (FormaClaveValor linea)

        "OBJETO" ->
            comprobar lineaObjeto (IdInvalido valor)

        "SALA" ->
            comprobar lineaSala (IdInvalido valor)

        "DESC" ->
            comprobar lineaDesc (FormaClaveValor linea)

        "SALIDA" ->
            comprobar lineaSalida (errorSalida linea valor)

        "PESO" ->
            comprobar lineaPeso (FormaClaveValor linea)

        _ ->
            [V1 numero (ClaveDesconocida nombre)]

    where
        comprobar :: ReadS a -> ErrorV1 -> [ErrorMundo]
        comprobar reconocedor errorV =
            case reconocerCompleto reconocedor linea of
                Just _  -> []
                Nothing -> [V1 numero errorV]

validarDesconocido :: Bloque -> [ErrorMundo]
validarDesconocido [] = []

validarDesconocido ((numero, linea):resto) =
    V1 numero errorEncabezado : concatMap revisar resto
    where
        conocidas =
            [ nombre
            | tipo <- [TCabecera, TObjeto, TSala]
            , (nombre, _, _) <- reglas tipo
            ]

        errorEncabezado =
            case partirClave linea of
                Just (nombre, _)
                    | nombre `notElem` conocidas ->
                        ClaveDesconocida nombre

                _ ->
                    FormaClaveValor
                        ("Encabezado de bloque inválido: " ++ linea)

        revisar actual@(n, texto) =
            case partirClave texto of
                Nothing ->
                    [V1 n (FormaClaveValor texto)]

                Just (nombre, valor)
                    | nombre `elem` conocidas ->
                        validarValor actual nombre valor

                    | otherwise ->
                        [V1 n (ClaveDesconocida nombre)]

tipoBloque :: Bloque -> Maybe TipoBloque
tipoBloque ((_, linea):_) =
    case partirClave linea of
        Just ("SALA", _)   -> Just TSala
        Just ("OBJETO", _) -> Just TObjeto
        _                  -> Nothing

tipoBloque [] = Nothing

salasAlcanzables :: Mundo -> Set.Set IdSala
salasAlcanzables mundo =
    recorrer [mundoInicio mundo] Set.empty
    where
        salas = mundoSalas mundo

        recorrer [] visitadas = visitadas

        recorrer (actual:pendientes) visitadas
            | Set.member actual visitadas =
                recorrer pendientes visitadas

            | otherwise =
                let definiciones =
                        Map.findWithDefault [] actual salas

                    destinos =
                        [ salidaDestino salida
                        | sala <- definiciones
                        , salidas <- Map.elems (salaSalidas sala)
                        , salida <- salidas
                        , Map.member (salidaDestino salida) salas
                        ]

                in recorrer (destinos ++ pendientes)
                           (Set.insert actual visitadas)

objetosNecesarios :: Mundo -> Set.Set IdObjeto
objetosNecesarios mundo =
    Set.fromList (requisitos ++ objetoMeta)
    where
        requisitos =
            [ objeto
            | definiciones <- Map.elems (mundoSalas mundo)
            , sala <- definiciones
            , salidas <- Map.elems (salaSalidas sala)
            , salida <- salidas
            , Just objeto <- [salidaRequisito salida]
            ]

        objetoMeta =
            case mundoMeta mundo of
                Llegar _ -> []
                Llevar objeto _ -> [objeto]

capacidadRequerida :: Mundo -> Int
capacidadRequerida mundo =
    max pesoMaximo pesoNecesarios
    where
        objetos =
            concat (Map.elems (mundoObjetos mundo))

        pesoMaximo =
            maximum (0 : map objetoPeso objetos)

        pesoNecesarios =
            sum
                [ objetoPeso objeto
                | idObj <- Set.toList (objetosNecesarios mundo)
                , Just (objeto:_) <- [Map.lookup idObj (mundoObjetos mundo)]
                ]

errorSalida :: String -> String -> ErrorV1
errorSalida linea valor =
    case words valor of
        (dir:_)
            | all (`elem` "abcdefghijklmnopqrstuvwxyzáéíóúñü0123456789_") dir
            , dir `notElem` direcciones
                -> DireccionInexistente dir

        _ -> SalidaMalFormada linea
    where
        direcciones =
            ["norte", "sur", "este", "oeste", "arriba", "abajo"]

-- Detecta separadores vacíos o repetidos
erroresSeparadores :: String -> [ErrorMundo]
erroresSeparadores texto =
    inicio ++ consecutivos ++ final
    where
        ls = prepararLineas texto

        inicio =
            case ls of
                ((n, "--"):_) ->
                    [V1 n (FormaClaveValor "Separador al inicio")]
                _ -> []

        consecutivos =
            [ V1 n (FormaClaveValor "Bloque vacío entre separadores")
            | ((_, a), (n, b)) <- zip ls (drop 1 ls)
            , a == "--"
            , b == "--"
            ]

        final =
            case reverse ls of
                ((n, "--"):_) ->
                    [V1 n (FormaClaveValor "Separador al final")]
                _ -> []