module Motor.Tipos
    ( Mundo(..)
    , Sala (..)
    , Objeto(..)
    , Direccion(..)
    , Meta(..)
    , Estado(..)
    , Comando(..)
    , ErrorMundo(..)
    , ErrorV1(..)
    , IdObjeto(..)
    , IdSala(..)
    ) where

import qualified Data.Map.Strict as Map

-- Identificador

newtype IdObjeto = IdObjeto String
    deriving (Eq, Ord)
instance Show IdObjeto where
    show (IdObjeto s) = s

newtype IdSala = IdSala String
    deriving (Eq, Ord)
instance Show IdSala where
    show (IdSala s) = s

-- Mundo

data Mundo = Mundo
    { mundoTitulo  :: String
    , mundoInicio  :: IdSala
    , mundoMeta    :: Meta
    , mundoSalas   :: Map.Map IdSala [Sala]
    , mundoObjetos :: Map.Map IdObjeto [Objeto]
    }

-- Sala

data Sala = Sala
    { salaId        :: IdSala
    , salaDesc      :: String
    , salaSalidas   :: Map.Map Direccion [Salida]
    , salaObjetos   :: [IdObjeto]
    } deriving (Eq, Show)

-- Objeto

data Objeto = Objeto
    { objetoId    :: IdObjeto
    , objetoDesc  :: String
    , objetoPeso  :: Int
    } deriving (Eq, Show)

-- Salida

data Salida = Salida
    { salidaDireccion :: Direccion
    , salidaDestino   :: IdSala
    , salidaRequisito :: Maybe IdObjeto
    } deriving (Eq, Ord, Show)

-- Direcciones

data Direccion
    = Norte
    | Sur
    | Este
    | Oeste
    | Arriba
    | Abajo
    deriving (Eq, Ord, Show)

-- Meta

data Meta
    = Llegar IdSala
    | Llevar IdObjeto IdSala

-- Estado

data Estado = Estado

-- Comando

data Comando = Comando

-- Errores

data ErrorMundo
    = ErrorSintactico   Int ErrorV1 -- V1
    | V2a   IdSala              -- Dos salas mismo id
    | V2b   IdObjeto            -- Dos objetos mismo id
    | V3    IdSala IdSala       -- Salida a sala no existente
    | V4    IdSala IdObjeto     -- Salida requiere objeto inexistente
    | V5    IdSala Direccion    -- Dos salidas en misma dirección
    | V6    IdSala IdObjeto     -- Sala contiene objeto no definido
    | V7    IdObjeto            -- Objeto repetido
    | V8a   IdSala              -- INICIO con sala inexistente
    | V8b   IdSala              -- META con sala inexistente
    | V8c   IdObjeto            -- META con objeto inexistente
    | V9    IdSala              -- Sala inalcanzable desde INICIO
    | V10   IdObjeto            -- Objeto necesario no ubicado
    deriving (Eq)

instance Show ErrorMundo where
    show (ErrorSintactico linea errorSyn)
        = "Error: V1 - línea " ++ show linea ++ ": " ++ mensajeErrorV1 errorSyn
    show (V2a sala)
        = "Error: V2 - Sala duplicada: " ++ show sala
    show (V2b objeto)
        = "Error: V2 - Objeto duplicado: " ++ show objeto
    show (V3 salaOrigen salaDestino)
        = "Error: V3 - La sala " ++ show salaOrigen
        ++ " tiene una salida hacia la sala inexistente " ++ show salaDestino
    show (V4 sala objeto)
        = "Error: V4 - Una salida de la sala " ++ show sala
        ++ " requiere el objeto inexistente " ++ show objeto
    show (V5 sala direccion)
        = "Error: V5 - La sala " ++ show sala
        ++ " tiene salidas repetidas en dirección " ++ show direccion
    show (V6 sala objeto)
        = "Error: V6 - La sala " ++ show sala
        ++ " contiene el objeto inexistente " ++ show objeto
    show (V7 objeto)
        = "Error: V7 - El objeto " ++ show objeto
        ++ " está colocado mas de una vez"
    show (V8a sala)
        = "Error: V8 - La sala inicial no existe: " ++ show sala
    show (V8b sala)
        = "Error: V8 - La sala objetivo no existe: " ++ show sala
    show (V8c objeto)
        = "Error: V8 - El objeto de la meta no existe: " ++ show objeto
    show (V9 sala)
        = "Error: V9 - La sala " ++ show sala
        ++ " no es alcanzable desde la sala inicial"
    show (V10 objeto)
        = "Error: El objeto necesario " ++ show objeto
        ++ " no esta colocado en ninguna sala"

data ErrorV1
    = ClaveDesconocida      String
    | FormaClaveValor       String
    | CampoAusente          String
    | CampoRepetido         String
    | SalidaMalFormada      String
    | MetaMalFormada        String
    | DireccionInexistente  String
    | IdInvalido            String
    deriving (Eq, Show)

mensajeErrorV1 :: ErrorV1 -> String
mensajeErrorV1 (ClaveDesconocida clave)   = "Clave desconocida " ++ clave
mensajeErrorV1 (FormaClaveValor texto)    = "Formato CLAVE: valor inválido: " ++ texto
mensajeErrorV1 (CampoAusente campo)       = "Campo obligatorio ausente: " ++ campo
mensajeErrorV1 (CampoRepetido campo)      = "Campo repetido: " ++ campo
mensajeErrorV1 (SalidaMalFormada salida)  = "Salida mal formada: " ++ salida
mensajeErrorV1 (MetaMalFormada meta)      = "Meta mal formada: " ++ meta
mensajeErrorV1 (DireccionInexistente dir) = "Dirección inexistente: " ++ dir
mensajeErrorV1 (IdInvalido iden)          = "Identificador inválido: " ++ iden