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

-- Reconocimiento de cabecera
leerCabecera :: ReadS Mundo
leerCabecera = undefined

lineaMundo :: ReadS String
lineaMundo m =
    [ (titulo, "")  | ("MUNDO", r1) <- lex m
                    , (":", r2) <- lex r1
                    , let titulo = dropWhile (== ' ') r2
    ]
        
lineaInicio :: ReadS IdSala
lineaInicio i =
    [ (IdSala sala, "")    | ("INICIO", r1) <- lex i
                    , (":", r2) <- lex r1
                    , let sala = dropWhile (== ' ') r2
    ]

lineaMeta :: ReadS Meta
lineaMeta = undefined

-- Reconocimiento de bloque de objeto
leerBloqObj :: ReadS Objeto
leerBloqObj = undefined

lineaObjeto :: ReadS IdObjeto
lineaObjeto o = 
    [ (IdObjeto objeto, "")  | ("OBJETO", r1) <- lex o
                    , (":", r2) <- lex r1
                    , let objeto = dropWhile (== ' ') r2
    ]  

-- Reconocimiento de bloque de sala
leerBLoqSala :: ReadS Sala
leerBLoqSala = undefined

lineaSala :: ReadS IdSala
lineaSala s = 
    [ (IdSala sala, "")    | ("SALA", r1) <- lex s
                    , (":", r2) <- lex r1
                    , let sala = dropWhile (== ' ') r2
    ]

lineaSalida :: ReadS Salida
lineaSalida = undefined

-- Funcion para las descripciones
lineaDesc :: ReadS String
lineaDesc d = 
    [ (descripcion, "") | ("DESC", r1) <- lex d
                        , (":", r2) <- lex r1
                        , let descripcion = dropWhile (== ' ') r2
    ]