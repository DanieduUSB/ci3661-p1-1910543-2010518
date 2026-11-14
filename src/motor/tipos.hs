module Motor.Tipos
    ( Mundo(..)
    , Sala (..)
    , Objeto(..)
    , Direccion(..)
    , Meta(..)
    , Estado(..)
    , Comando(..)
    , ErrorMundo(..)
    , ErrorSyntax(..)
    , ErrorSintactico
    , reportarErrorSyntax
    ) where

-- Identificador

newtype IdObjeto = IdObjeto String
    deriving (Eq, Ord, Show)

newtype IdSala = IdSala String
    deriving (Eq, Ord, Show)

-- Mundo

data Mundo = Mundo
    { titulo :: String
    , inicio :: IdSala
    , meta   :: Meta
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

data ErrorMundo = ErrorMundo

data ErrorSintactico = ErrorSintactico Int ErrorSyntax
    deriving (Eq)

instance Show ErrorSintactico where
    show (ErrorSintactico linea errorSyn) =
        "Error: línea " ++ show linea ++ ": " ++ mensajeError errorSyn

data ErrorSyntax
    = ClaveDesconocida String
    | FormaClaveValor String
    | CampoAusente String
    | CampoRepetido String
    | SalidaMalFormada String
    | MetaMalFormada String
    | DireccionInexistente String
    | IdInvalido String
    deriving (Eq, Show)

mensajeError :: ErrorSyntax -> String
mensajeError (ClaveDesconocida clave)   = "Clave desconocida '" ++ clave ++ "'"
mensajeError (FormaClaveValor texto)    = "Formato CLAVE: valor inválido: '" ++ texto ++ "'"
mensajeError (CampoAusente campo)       = "Campo obligatorio ausente: " ++ campo ++ "'"
mensajeError (CampoRepetido campo)      = "Campo repetido: '" ++ campo ++ "'"
mensajeError (SalidaMalFormada salida)  = "Salida mal formada: '" ++ salida ++ "'"
mensajeError (MetaMalFormada meta)      = "Meta mal formada: '" ++ meta ++ "'"
mensajeError (DireccionInexistente dir) = "Dirección inexistente: '" ++ dir ++ "'"
mensajeError (IdInvalido iden)          = "Identificador inválido: '" ++ iden ++ "'"

reportarErrorSyntax :: ErrorSyntax -> Int -> ErrorSintactico
reportarErrorSyntax errorSyn linea = ErrorSintactico linea errorSyn