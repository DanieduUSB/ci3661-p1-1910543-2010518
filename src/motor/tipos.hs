module Motor.Tipos
    ( Mundo(..)
    , Sala (..)
    , Objeto(..)
    , Direccion(..)
    , Meta(..)
    , Estado(..)
    , Comando(..)
    , ErrorMundo(..)
    , reportarErrorV1
    , IdObjeto(..)
    , IdSala(..)
    ) where

-- Identificador

newtype IdObjeto = IdObjeto String
    deriving (Eq, Ord, Show)

newtype IdSala = IdSala String
    deriving (Eq, Ord, Show)

-- Mundo

data Mundo = Mundo
    { mundoTitulo  :: String
    , mundoInicio  :: IdSala
    , mundoMeta    :: Meta
    , mundoSalas   :: [Sala]
    , mundoObjetos :: [Objeto]
    }

-- Sala

data Sala = Sala
    { salaId        :: IdSala
    , salaDesc      :: String
    , salaSalidas   :: [Salida]
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
    = ErrorSintactico               Int ErrorV1
    | SalaDuplicada                 IdSala -- V2
    | ObjetoDuplicado               IdObjeto -- V2
    | SalaDestinoInexistente        IdSala IdSala -- V3
    | ObjetoRequeridoInexistente    IdSala IdObjeto -- V4
    | DireccionDuplicada            IdSala Direccion -- V5
    | ObjetoInexistenteEnSala       IdSala IdObjeto -- V6
    | ObjetoRepetido                IdObjeto -- V7
    | SalaInicioInexistente         IdSala -- V8
    | SalaMetaInexistente           IdSala -- V8
    | ObjetoMetaInexistente         IdObjeto -- V8
    | SalaInalcanzable              IdSala -- V9
    | ObjetoNecesarioSinUbicacion   IdObjeto -- V10
    deriving (Eq)

instance Show ErrorMundo where
    show (ErrorSintactico linea errorSyn) =
        "Error: línea " ++ show linea ++ ": " ++ mensajeErrorV1 errorSyn
    show _ = "Error: "

data ErrorV1
    = ClaveDesconocida String
    | FormaClaveValor String
    | CampoAusente String
    | CampoRepetido String
    | SalidaMalFormada String
    | MetaMalFormada String
    | DireccionInexistente String
    | IdInvalido String
    deriving (Eq, Show)

mensajeErrorV1 :: ErrorV1 -> String
mensajeErrorV1 (ClaveDesconocida clave)   = "Clave desconocida '" ++ clave ++ "'"
mensajeErrorV1 (FormaClaveValor texto)    = "Formato CLAVE: valor inválido: '" ++ texto ++ "'"
mensajeErrorV1 (CampoAusente campo)       = "Campo obligatorio ausente: " ++ campo ++ "'"
mensajeErrorV1 (CampoRepetido campo)      = "Campo repetido: '" ++ campo ++ "'"
mensajeErrorV1 (SalidaMalFormada salida)  = "Salida mal formada: '" ++ salida ++ "'"
mensajeErrorV1 (MetaMalFormada meta)      = "Meta mal formada: '" ++ meta ++ "'"
mensajeErrorV1 (DireccionInexistente dir) = "Dirección inexistente: '" ++ dir ++ "'"
mensajeErrorV1 (IdInvalido iden)          = "Identificador inválido: '" ++ iden ++ "'"

reportarErrorV1 :: ErrorV1 -> Int -> ErrorMundo
reportarErrorV1 errorSyn linea = ErrorSintactico linea errorSyn