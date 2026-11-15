module Motor.Reconocedor
    ( leerCabecera
    , lineaMundo
    , lineaInicio
    , lineaMeta
    , leerBloqObj
    , lineaObjeto
    , leerBloqSala
    , lineaSala
    , lineaDesc
    , lineaSalida
    , lineaCapacidad
    , lineaPeso
) where

import Motor.Tipos

import Data.List (stripPrefix, dropWhileEnd, isPrefixOf)
import Data.Char (isSpace)

-- Reconocimiento de cabecera
leerCabecera :: ReadS Mundo
leerCabecera = undefined

lineaMundo :: ReadS String
lineaMundo m =
    [ (titulo, "")
    | ((), resto) <- clave "MUNDO" m
    , let titulo = limpiar resto
    , not (null titulo)
    ]
        
lineaInicio :: ReadS IdSala
lineaInicio i =
    [ (IdSala sala, resto)
    | ((), r1) <- clave "INICIO" i
    , (sala, resto) <- identificador r1
    ]

lineaMeta :: ReadS Meta
lineaMeta m =
    [ (meta, resto)
    | ((), r1) <- clave "META" m
    , (meta, resto) <- alternativa metaSinObjeto metaConObjeto r1
    ]

-- Reconoce la capacidad máxima
lineaCapacidad :: ReadS Int
lineaCapacidad s =
    [ (capacidad, resto)
    | ((), r1) <- clave "CAPACIDAD" s
    , (capacidad, resto) <- numeroPositivo r1
    ]

metaSinObjeto :: ReadS Meta
metaSinObjeto s =
    [ (Llegar (IdSala sala), resto)
    | (_, r1) <- palabra "llegar" s
    , (_, r2) <- palabra "a" r1
    , (sala, resto) <- identificador r2
    ]

metaConObjeto :: ReadS Meta
metaConObjeto c =
    [ (Llevar (IdObjeto objeto) (IdSala sala), resto)
    | (_, r1) <- palabra "llevar" c
    , (objeto, r2) <- identificador r1
    , (_, r3) <- palabra "a" r2
    , (sala, resto) <- identificador r3
    ]

-- Reconocimiento de bloque de objeto
leerBloqObj :: ReadS Objeto
leerBloqObj texto =
    [ (Objeto objeto descripcion peso, "")
    | let ls = map limpiar (lines texto)

    -- Primera linea: OBJETO
    , (primera:resto) <- [ls]
    , Just objeto <- [reconocerCompleto lineaObjeto primera]

    -- Campo obligatorio: DESC
    , let descripciones = filter (isPrefixOf "DESC:") resto
    , [lineaD] <- [descripciones]
    , Just descripcion <- [reconocerCompleto lineaDesc lineaD]

    -- Campo opcional: PESO
    , let pesos = filter (isPrefixOf "PESO:") resto
    , peso <- case pesos of
        []  -> [1]
        [p] -> case reconocerCompleto lineaPeso p of
            Just n  -> [n]
            Nothing -> []
        _   -> []

    , length resto == length descripciones + length pesos
    ]

lineaObjeto :: ReadS IdObjeto
lineaObjeto o =
    [ (IdObjeto objeto, resto)
    | ((), r1) <- clave "OBJETO" o
    , (objeto, resto) <- identificador r1
    ]

-- Reconoce el peso de un objeto
lineaPeso :: ReadS Int
lineaPeso s =
    [ (peso, resto)
    | ((), r1) <- clave "PESO" s
    , (peso, resto) <- numeroPositivo r1
    ]

-- Reconocimiento de bloque de sala
leerBloqSala :: ReadS Sala
leerBloqSala texto =
    [ (Sala sala descripcion salidas objetos, "")
    | let ls = map limpiar (lines texto)

    -- Primera linea: SALA
    , (primera:resto) <- [ls]
    , Just sala <- [reconocerCompleto lineaSala primera]

    -- Campo obligatorio: DESC
    , let descripciones = filter (isPrefixOf "DESC:") resto
    , [lineaD] <- [descripciones]
    , Just descripcion <- [reconocerCompleto lineaDesc lineaD]

    -- Salidas de la sala
    , let lineasSalida = filter (isPrefixOf "SALIDA:") resto
    , Just salidas <- [mapM (reconocerCompleto lineaSalida) lineasSalida]

    -- Objetos de la sala
    , let lineasObjeto = filter (isPrefixOf "OBJETO:") resto
    , Just objetos <- [mapM (reconocerCompleto lineaObjeto) lineasObjeto]

    -- Verificar que no haya campos adicionales
    , length resto ==
        length descripciones + length lineasSalida + length lineasObjeto
    ]

lineaSala :: ReadS IdSala
lineaSala s =
    [ (IdSala sala, resto)
    | ((), r1) <- clave "SALA" s
    , (sala, resto) <- identificador r1
    ]

lineaSalida :: ReadS Salida
lineaSalida s =
    [ (salida, resto)
    | ((), r1) <- clave "SALIDA" s
    , (salida, resto) <-
        alternativa salidaConObjeto salidaSinObjeto r1
    ]

salidaSinObjeto :: ReadS Salida
salidaSinObjeto s =
    [ (Salida dir (IdSala destino) Nothing, resto)
    | (dir, r1) <- direccion s
    , (_, r2) <- palabra "->" r1
    , (destino, resto) <- identificador r2
    ]

salidaConObjeto :: ReadS Salida
salidaConObjeto s =
    [ (Salida dir (IdSala destino)
              (Just (IdObjeto objeto)), resto)
    | (dir, r1) <- direccion s
    , (_, r2) <- palabra "->" r1
    , (destino, r3) <- identificador r2
    , (_, r4) <- palabra "requiere" r3
    , (objeto, resto) <- identificador r4
    ]

-- Funcion para las descripciones
lineaDesc :: ReadS String
lineaDesc d =
    [ (descripcion, "") 
    | ((), resto) <- clave "DESC" d
    , let descripcion = limpiar resto
    , not (null descripcion)
    ]

-- RECONOCEDORES

-- Reconoce un texto literal
literal :: String -> ReadS String
literal esperado s =
    [ (esperado, resto)
    | Just resto <- [stripPrefix esperado s]
    ]

-- Reconoce una clave seguida de :
clave :: String -> ReadS ()
clave nombre s =
    [ ((), resto)
    | (_, resto) <- literal (nombre ++ ":") s
    ]

-- Reconoce una palabra o símbolo mediante lex
palabra :: String -> ReadS String
palabra esperado s =
    [ (token, resto)
    | (token, resto) <- lex s
    , token == esperado
    ]

-- Reconoce números enteros positivos
numeroPositivo :: ReadS Int
numeroPositivo s =
    [ (numero, resto)
    | (numero, resto) <- (reads s :: [(Int, String)])
    , numero > 0
    ]

-- Función auxiliar para limpiar espacios
limpiar :: String -> String
limpiar = dropWhileEnd isSpace . dropWhile isSpace

-- Comprueba si un carácter es válido para un identificador
caracterId :: Char -> Bool
caracterId c =
    (c >= 'a' && c <= 'z')
    || c `elem` "áéíóúñü0123456789_"

-- Reconoce un identificador
identificador :: ReadS String
identificador s =
    [ (nombre, resto)
    | let entrada = dropWhile isSpace s
    , let (nombre, resto) = span caracterId entrada
    , not (null nombre)
    ]

-- Reconoce una dirección
direccion :: ReadS Direccion
direccion s =
    [ (dir, resto)
    | (nombre, dir) <- direcciones
    , (_, resto) <- palabra nombre s
    ]
    where
        direcciones =
            [ ("norte", Norte)
            , ("sur", Sur)
            , ("este", Este)
            , ("oeste", Oeste)
            , ("arriba", Arriba)
            , ("abajo", Abajo)
            ]

-- Función auxiliar que combina dos ReadS
alternativa :: ReadS a -> ReadS a -> ReadS a
alternativa p q s = p s ++ q s

-- Comprueba que el reconocimiento consumió toda la entrada
reconocerCompleto :: ReadS a -> String -> Maybe a
reconocerCompleto reconocedor entrada =
    case [valor | (valor, resto) <- reconocedor entrada
                , all isSpace resto] of
        [valor] -> Just valor
        _       -> Nothing