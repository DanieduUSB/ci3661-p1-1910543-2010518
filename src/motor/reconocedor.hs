module Motor.Reconocedor
    ( leerCabecera
    , lineaMun
    , lineaInicio
    , lineaMeta
    , leerBloqObj
    , lineaObjeto
    , leerBLoqSala
    , lineaSala
    , lineaDesc
    , lineaSalida
    , lineaObjSala
) where

import Motor.Tipos

-- Reconocimiento de cabecera

leerCabecera :: ReadS String
leerCabecera = undefined

lineaMun :: ReadS String
lineaMun = undefined
        
lineaInicio :: ReadS IdSala
lineaInicio x = undefined

lineaMeta :: ReadS String
lineaMeta = undefined

-- Reconocimiento de bloque de objeto

leerBloqObj :: ReadS String
leerBloqObj = undefined

lineaObjeto :: ReadS IdObjeto
lineaObjeto = undefined

-- Reconocimiento de bloque de sala

leerBLoqSala :: ReadS String
leerBLoqSala = undefined

lineaSala :: ReadS IdSala
lineaSala = undefined

lineaSalida :: ReadS String
lineaSalida = undefined

-- Funcion para las descripciones
lineaDesc :: ReadS String
lineaDesc = undefined