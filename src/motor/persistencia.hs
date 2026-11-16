module Motor.Persistencia
    ( leerArchivo
    , escribirArchivo
    ) where

leerArchivo :: FilePath -> IO (Either String String)
leerArchivo = undefined

escribirArchivo :: FilePath -> String -> IO (Either String ())
escribirArchivo = undefined