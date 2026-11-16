module Datos.MundoPrueba
    ( mundoMetaConRequisito
    , mundoMetaSinRequisito
    ) where

import Motor.Tipos
import Motor.Nucleo

------------------------------
-- ESTRUCTURAS PARA PRUEBAS --
------------------------------
-- Escenario: Edificio con PB y 2 pisos,
-- 2 apartamentos por piso y salida a
-- la azotea.

-- El objetivo es llegar a la azotea con un paracaidas
mundoMetaConRequisito :: Mundo
mundoMetaConRequisito = undefined

-- El objetivo es llegar a la azotea
mundoMetaSinRequisito :: Mundo
mundoMetaSinRequisito = undefined

-- Objetos: 
objetoLlave :: Objeto
objetoLlave = Objeto
    { objetoId      = IdObjeto "llave"
    , objetoDesc    = "Empieza por '1_', lo demás se borró"
    , objetoPeso    = 2
    }

objetoLlaveMagnetica :: Objeto
objetoLlaveMagnetica = Objeto
    { objetoId      = IdObjeto "llave_magnetica"
    , objetoDesc    = "Una llave para una cerradura magnética. Me pregunto si estará codificada para "
        ++ "algún edificio cercano..."
    , objetoPeso    = 2
    }

objetoParacaidas :: Objeto
objetoParacaidas = Objeto
    { objetoId      = IdObjeto "paracaidas"
    , objetoDesc    = "Parece funcional, y es un modelo arrecho que se abre rápido "
        ++ "en caídas cortas, me encantaría probarlo"
    , objetoPeso    = 4
    }

objetoTrebol :: Objeto
objetoTrebol = Objeto
    { objetoId      = IdObjeto "trebol_cuatro_hojas"
    , objetoDesc    = "Parece que mi suerte cambiará, Willie Colón tenía razón"
    , objetoPeso    = 1
    }

objetoCondonUsado :: Objeto
objetoCondonUsado = Objeto
    { objetoId      = IdObjeto "condon_usado"
    , objetoDesc    = "Que asco"
    , objetoPeso    = 1
    }

objetoPalanca :: Objeto
objetoPalanca = Objeto
    { objetoId      = IdObjeto "palanca"
    , objetoDesc    = "Si necesito abrir algo que esté trancado me sería útil"
    , objetoPeso    = 3
    }

-- Salidas:
salidaCallePb :: Salida
salidaCallePb = Salida
    { salidaDireccion   = Norte
    , salidaDestino     = IdSala "pb"
    , salidaRequisito   = Just (IdObjeto "llave_magnetica")
    }

salidaPbCalle :: Salida
salidaPbCalle = Salida
    { salidaDireccion   = Sur
    , salidaDestino     = IdSala "calle"
    , salidaRequisito   = Just (IdObjeto "llave_magnetica")
    }



salidaPbPiso1 :: Salida
salidaPbPiso1 = Salida
    { salidaDireccion   = Arriba
    , salidaDestino     = IdSala "piso_1"
    , salidaRequisito   = Nothing
    }

salidaPiso1Pb :: Salida
salidaPiso1Pb = Salida
    { salidaDireccion   = Abajo
    , salidaDestino     = IdSala "pb"
    , salidaRequisito   = Nothing
    }



salidaPiso1Apto1a :: Salida
salidaPiso1Apto1a = Salida
    { salidaDireccion   = Este
    , salidaDestino     = IdSala "apto_1_a"
    , salidaRequisito   = Just (IdObjeto "llave")
    }

salidaApto1aPiso1 :: Salida
salidaApto1aPiso1 = Salida
    { salidaDireccion   = Oeste
    , salidaDestino     = IdSala "piso_1"
    , salidaRequisito   = Nothing
    }



salidaPiso1Apto1b :: Salida
salidaPiso1Apto1b = Salida
    { salidaDireccion   = Oeste
    , salidaDestino     = IdSala "apto_1_b"
    , salidaRequisito   = Nothing
    }

salidaApto1bPiso1 :: Salida
salidaApto1bPiso1 = Salida
    { salidaDireccion   = Este
    , salidaDestino     = IdSala "piso_1"
    , salidaRequisito   = Nothing
    }



salidaPiso1Piso2 :: Salida
salidaPiso1Piso2 = Salida
    { salidaDireccion   = Arriba
    , salidaDestino     = IdSala "piso_2"
    , salidaRequisito   = Nothing
    }

salidaPiso2Piso1 :: Salida
salidaPiso2Piso1 = Salida
    { salidaDireccion   = Abajo
    , salidaDestino     = IdSala "piso_1"
    , salidaRequisito   = Nothing
    }



salidaPiso2Apto2a :: Salida
salidaPiso2Apto2a = Salida
    { salidaDireccion   = Este
    , salidaDestino     = IdSala "apto_2_a"
    , salidaRequisito   = Nothing
    }

salidaApto2aPiso2 :: Salida
salidaApto2aPiso2 = Salida
    { salidaDireccion   = Oeste
    , salidaDestino     = IdSala "piso_2"
    , salidaRequisito   = Nothing
    }



salidaPiso2Apto2b :: Salida
salidaPiso2Apto2b = Salida
    { salidaDireccion   = Oeste
    , salidaDestino     = IdSala "apto_2_b"
    , salidaRequisito   = Nothing
    }



salidaPiso2Azotea :: Salida
salidaPiso2Azotea = Salida
    { salidaDireccion   = Arriba
    , salidaDestino     = IdSala "azotea"
    , salidaRequisito   = Just (IdObjeto "palanca")
    }

salidaAzoteaPiso2 :: Salida
salidaAzoteaPiso2 = Salida
    { salidaDireccion   = Abajo
    , salidaDestino     = IdSala "piso_2"
    , salidaRequisito   = Nothing
    }