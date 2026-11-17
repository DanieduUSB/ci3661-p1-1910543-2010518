-- Pruebas HUnit para Motor.Mundo.
module Mundo (testsMundo) where

import Test.HUnit

import Motor.Mundo (leerMundo, prepararLineas)
import Motor.Tipos
import Data.List (sort)
import qualified Data.Map.Strict as Map

type Prueba = (String, Bool, String)

cabecera :: String -> String -> Int -> [String]
cabecera inicio meta capacidad =
    [ "MUNDO: Prueba", "INICIO: " ++ inicio
    , "META: " ++ meta, "CAPACIDAD: " ++ show capacidad
    ]

sala :: String -> [String] -> [String]
sala nombre extras = ["SALA: " ++ nombre, "DESC: Sala " ++ nombre] ++ extras

objeto :: String -> Int -> [String]
objeto nombre peso = ["OBJETO: " ++ nombre, "DESC: Objeto " ++ nombre, "PESO: " ++ show peso]

mundo :: [String] -> [[String]] -> String
mundo cab bloques = unlines (cab ++ concatMap ("--" :) bloques)

base :: String
base = mundo (cabecera "entrada" "llegar a entrada" 10) [sala "entrada" []]

categoriaV1 :: ErrorV1 -> String
categoriaV1 (ClaveDesconocida _) = "ClaveDesconocida"
categoriaV1 (FormaClaveValor _) = "FormaClaveValor"
categoriaV1 (CampoAusente _) = "CampoAusente"
categoriaV1 (CampoRepetido _) = "CampoRepetido"
categoriaV1 (SalidaMalFormada _) = "SalidaMalFormada"
categoriaV1 (MetaMalFormada _) = "MetaMalFormada"
categoriaV1 (DireccionInexistente _) = "DireccionInexistente"
categoriaV1 (IdInvalido _) = "IdInvalido"

categoria :: ErrorMundo -> String
categoria (V1 _ e) = "V1:" ++ categoriaV1 e
categoria (V2a _) = "V2a"
categoria (V2b _) = "V2b"
categoria (V3 _ _) = "V3"
categoria (V4 _ _) = "V4"
categoria (V5 _ _) = "V5"
categoria (V6 _ _) = "V6"
categoria (V7 _) = "V7"
categoria (V8a _) = "V8a"
categoria (V8b _) = "V8b"
categoria (V8c _) = "V8c"
categoria (V9 _) = "V9"
categoria (V10 _) = "V10"
categoria (PesoExcesivo _ _ _) = "PesoExcesivo"
categoria (CapacidadInsuficiente _ _) = "CapacidadInsuficiente"

-- Espera exactamente estas categorias, incluyendo la cantidad de errores.
verificar :: String -> [String] -> String -> Prueba
verificar nombre esperadas entrada =
    let obtenidas = case leerMundo entrada of
            Right _ -> []
            Left errores -> sort (map categoria errores)
        esperadasOrdenadas = sort esperadas
    in (nombre, obtenidas == esperadasOrdenadas,
        "\n       Esperado: " ++ show esperadasOrdenadas ++
        "\n       Obtenido: " ++ show obtenidas)

verificarCondicion :: String -> Bool -> Prueba
verificarCondicion nombre resultado = (nombre, resultado, "")

pruebas :: [Prueba]
pruebas =
    [ verificar "01 Mundo minimo valido" [] base
    , verificar "02 Peso por defecto (mundo valido)" []
        (mundo (cabecera "entrada" "llegar a entrada" 1)
            [sala "entrada" ["OBJETO: llave"], ["OBJETO: llave", "DESC: Una llave"]])
    , verificar "03 V1: clave desconocida" ["V1:ClaveDesconocida"]
        (mundo (cabecera "entrada" "llegar a entrada" 10) [sala "entrada" ["COLOR: rojo"]])
    , verificar "04 V1: falta dos puntos" ["V1:FormaClaveValor"]
        (mundo (cabecera "entrada" "llegar a entrada" 10) [sala "entrada" ["algo raro"]])
    , verificar "05 V1: identificador inicial invalido" ["V1:IdInvalido"]
        (mundo (cabecera "Entrada" "llegar a entrada" 10) [sala "entrada" []])
    , verificar "06 V1: falta DESC en objeto" ["V1:CampoAusente"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" [], ["OBJETO: llave", "PESO: 1"]])
    , verificar "07 V1: DESC repetido" ["V1:CampoRepetido"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["DESC: Otra descripcion"]])
    , verificar "08 V1: direccion inexistente" ["V1:DireccionInexistente"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: diagonal -> entrada"]])
    , verificar "09 V1: SALIDA mal formada" ["V1:SalidaMalFormada"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: norte entrada"]])
    , verificar "10 V1: META mal formada" ["V1:MetaMalFormada"]
        (mundo (cabecera "entrada" "llevar llave entrada" 10) [sala "entrada" []])
    , verificar "11 V1: capacidad no positiva" ["V1:FormaClaveValor"]
        (mundo (cabecera "entrada" "llegar a entrada" 0) [sala "entrada" []])
    , verificar "12 V1: peso no positivo" ["V1:FormaClaveValor"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" [], objeto "llave" 0])
    , verificar "13 V1: bloque desconocido" ["V1:ClaveDesconocida"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" [], ["PERSONA: juan", "DESC: Una persona"]])
    , verificar "14 V1: separador al final" ["V1:FormaClaveValor"]
        (base ++ "--\n")
    , verificar "15 V1: tres errores simultaneos"
        ["V1:IdInvalido", "V1:ClaveDesconocida", "V1:DireccionInexistente"]
        (mundo (cabecera "Entrada" "llegar a entrada" 10)
            [sala "entrada" ["COLOR: rojo", "SALIDA: diagonal -> entrada"]])
    , verificar "16 V2: sala duplicada" ["V2a"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" [], sala "entrada" []])
    , verificar "17 V2: objeto duplicado" ["V2b"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" [], objeto "llave" 1, objeto "llave" 1])
    , verificar "18 V3: destino inexistente" ["V3"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: norte -> fantasma"]])
    , verificar "19 V4: requisito inexistente" ["V4"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: norte -> entrada requiere llave"]])
    , verificar "20 V5: salida repetida" ["V5"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: norte -> entrada", "SALIDA: norte -> entrada"]])
    , verificar "21 V6: objeto no definido en sala" ["V6"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["OBJETO: llave"]])
    , verificar "22 V7: doble colocacion misma sala" ["V7"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["OBJETO: llave", "OBJETO: llave"], objeto "llave" 1])
    , verificar "23 V7: colocacion en dos salas" ["V7"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: norte -> patio", "OBJETO: llave"]
            , sala "patio" ["OBJETO: llave"], objeto "llave" 1])
    , verificar "24 V8a: inicio inexistente" ["V8a"]
        (mundo (cabecera "fantasma" "llegar a entrada" 10) [sala "entrada" []])
    , verificar "25 V8b: meta de sala inexistente" ["V8b"]
        (mundo (cabecera "entrada" "llegar a fantasma" 10) [sala "entrada" []])
    , verificar "26 V8c: meta de objeto inexistente" ["V8c"]
        (mundo (cabecera "entrada" "llevar llave a entrada" 10) [sala "entrada" []])
    , verificar "27 V9: sala inalcanzable" ["V9"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" [], sala "patio" ["SALIDA: sur -> entrada"]])
    , verificar "28 V10: objeto de meta sin colocar" ["V10"]
        (mundo (cabecera "entrada" "llevar llave a entrada" 10)
            [sala "entrada" [], objeto "llave" 1])
    , verificar "29 V10: requisito sin colocar" ["V10"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: norte -> entrada requiere llave"], objeto "llave" 1])
    , verificar "30 Peso: objeto que supera capacidad (sin error redundante)"
        ["PesoExcesivo"]
        (mundo (cabecera "entrada" "llegar a entrada" 3)
            [sala "entrada" [], objeto "piedra" 5])
    , verificar "31 Peso: suma necesaria supera capacidad" ["CapacidadInsuficiente"]
        (mundo (cabecera "entrada" "llevar carnet a entrada" 4)
            [sala "entrada" ["SALIDA: norte -> entrada requiere llave", "OBJETO: llave", "OBJETO: carnet"]
            , objeto "llave" 2, objeto "carnet" 3])
    , verificar "32 Peso: objeto usado en meta y salida se cuenta una vez" []
        (mundo (cabecera "entrada" "llevar llave a entrada" 4)
            [sala "entrada" ["SALIDA: norte -> entrada requiere llave", "OBJETO: llave"]
            , objeto "llave" 4])
    , verificar "33 Nombres iguales de sala y objeto estan permitidos" []
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["OBJETO: entrada"], objeto "entrada" 1])
    , verificar "34 UTF8 en identificadores" []
        (mundo (cabecera "sótano" "llegar a sótano" 2) [sala "sótano" []])
    , verificar "35 Comentarios en lineas (extension del equipo)" []
        (unlines ["# mundo", "MUNDO: Demo # comentario", "INICIO: entrada", "META: llegar a entrada"
                 , "CAPACIDAD: 2", "-- # separador", "SALA: entrada", "DESC: Sala # comentario"])
    , verificar "36 V2 + V3 + V4 + V5 simultaneos" ["V2a", "V3", "V4", "V5"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["SALIDA: norte -> fantasma", "SALIDA: norte -> entrada requiere llave"]
            , sala "entrada" []])
    , verificarCondicion "37 Numero de linea original tras comentarios"
        (case leerMundo (unlines ["# comentario", "MUNDO: Prueba", "INICIO: Entrada"
                                   , "META: llegar a entrada", "CAPACIDAD: 2", "--", "SALA: entrada", "DESC: Sala"]) of
            Left errores -> V1 3 (IdInvalido " Entrada") `elem` errores
            _ -> False)
    , verificarCondicion "38 Peso por defecto es exactamente 1"
        (case leerMundo (mundo (cabecera "entrada" "llegar a entrada" 1)
            [sala "entrada" ["OBJETO: llave"], ["OBJETO: llave", "DESC: Una llave"]]) of
            Right m -> case Map.lookup (IdObjeto "llave") (mundoObjetos m) of
                Just [o] -> objetoPeso o == 1
                _ -> False
            _ -> False)
    , verificarCondicion "39 Mundo vacio produce solo V1"
        (case leerMundo "" of
            Left errores -> not (null errores) && all esV1 errores
            _ -> False)
    , verificarCondicion "40 Una linea mal formada conserva su numero"
        (case leerMundo (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" ["objeto invalido"]]) of
            Left errores -> V1 8 (FormaClaveValor "objeto invalido") `elem` errores
            _ -> False)
    , verificar "41 V1: META repetida" ["V1:CampoRepetido"]
        (mundo (cabecera "entrada" "llegar a entrada" 10 ++ ["META: llegar a entrada"])
            [sala "entrada" []])
    , verificar "42 V1: INICIO ausente" ["V1:CampoAusente"]
        (mundo ["MUNDO: Prueba", "META: llegar a entrada", "CAPACIDAD: 10"] [sala "entrada" []])
    , verificar "43 V1: objeto con ID invalido" ["V1:IdInvalido"]
        (mundo (cabecera "entrada" "llegar a entrada" 10)
            [sala "entrada" [], ["OBJETO: Llave", "DESC: Una llave"]])
    , verificar "44 V1: separadores consecutivos" ["V1:FormaClaveValor"]
        (unlines (cabecera "entrada" "llegar a entrada" 10 ++ ["--", "--"] ++ sala "entrada" []))
    , verificarCondicion "45 Comentarios conservan el numero de linea"
        (prepararLineas "# comentario\n  OBJETO: llave # nota\n" == [(2, "OBJETO: llave")])
    ]

esV1 :: ErrorMundo -> Bool
esV1 (V1 _ _) = True
esV1 _ = False

-- Cada caso se transforma en una prueba individual de HUnit.
-- Si falla, muestra las categorias esperadas y obtenidas.
testsMundo :: Test
testsMundo = TestList
    [ TestLabel nombre (TestCase (assertBool detalle correcto))
    | (nombre, correcto, detalle) <- pruebas
    ]
