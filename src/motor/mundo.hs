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
                            let mundo = mundoCabecera
                                    { mundoSalas = agruparPor salaId salas
                                    , mundoObjetos = agruparPor objetoId objetos
                                    }

                                erroresV2 = validarV2 mundo

                            in case erroresV2 of
                                [] -> Right mundo
                                _  -> Left erroresV2

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
    , ("CAPACIDAD", True, False)
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

errorSalida :: String -> String -> ErrorV1
errorSalida linea valor =
    case words valor of
        (dir:_)
            | dir `notElem`
                ["norte", "sur", "este", "oeste", "arriba", "abajo"]
                -> DireccionInexistente dir

        _ -> SalidaMalFormada linea

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