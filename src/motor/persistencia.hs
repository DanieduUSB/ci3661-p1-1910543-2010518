module Motor.Persistencia
    ( leerArchivo
    , escribirArchivo
    ) where
    
import Control.Exception (try, IOException, evaluate)

leerArchivo :: FilePath -> IO (Either String String)
leerArchivo ruta = do
    resultado <- try (do
        contenido <- readFile ruta
        _ <- evaluate (length contenido)
        return contenido
        ) :: IO (Either IOException String)
    case resultado of
        Left err        -> return (Left (show err))
        Right contenido -> return (Right contenido)

escribirArchivo :: FilePath -> String -> IO (Either String ())
escribirArchivo = undefined