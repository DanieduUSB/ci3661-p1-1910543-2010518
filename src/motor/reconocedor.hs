module Motor.Reconocedor
    ( leerCabecera
    , lineaMundo
    , lineaInicio
    , lineaMeta
    , leerBloqObj
    , lineaObjeto
    , leerBLoqSala
    , lineaSala
    , lineaDesc
    , lineaSalida
) where

import Motor.Tipos

import Data.List (stripPrefix, dropWhileEnd)
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
lineaMeta = undefined

-- Reconocimiento de bloque de objeto
leerBloqObj :: ReadS Objeto
leerBloqObj = undefined

lineaObjeto :: ReadS IdObjeto
lineaObjeto o =
    [ (IdObjeto objeto, resto)
    | ((), r1) <- clave "OBJETO" o
    , (objeto, resto) <- identificador r1
    ]

-- Reconocimiento de bloque de sala
leerBLoqSala :: ReadS Sala
leerBLoqSala = undefined

lineaSala :: ReadS IdSala
lineaSala s =
    [ (IdSala sala, resto)
    | ((), r1) <- clave "SALA" s
    , (sala, resto) <- identificador r1
    ]

lineaSalida :: ReadS Salida
lineaSalida = undefined

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