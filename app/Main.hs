module Main (main) where

import Control.Monad (unless, when)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, hIsTerminalDevice, isEOF, stdin, stdout)

import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (iniciarPartida, ejecutar, verificarVictoriaInicial)
import Motor.Persistencia (escribirArchivo, leerArchivo)
import Motor.Serializador (deserializarEstado, serializarEstado)
import Motor.Tipos (Comando(..), Estado(..), Mundo)

-- Interpreta los argumentos del ejecutable.
main :: IO ()
main = do
    argumentos <- getArgs
    case argumentos of
        ["--validar", ruta] -> validarArchivo ruta
        ["-validar", ruta]  -> validarArchivo ruta
        [ruta]              -> iniciarJuego ruta
        _ -> do
            putStrLn "Uso: aventura MUNDO"
            putStrLn "     aventura --validar MUNDO"
            exitFailure

-- Lee, reconoce y valida el mundo. Ante un error informa y termina con codigo 1.
cargarMundo :: FilePath -> IO Mundo
cargarMundo ruta = do
    resultado <- leerArchivo ruta
    case resultado of
        Left detalle -> do
            putStrLn ("Error: No se pudo leer el archivo " ++ ruta ++ ": " ++ detalle)
            exitFailure
        Right texto ->
            case leerMundo texto of
                Left errores -> do
                    mapM_ (putStrLn . show) errores
                    exitFailure
                Right mundo -> return mundo

-- Modo --validar: no comienza una partida.
validarArchivo :: FilePath -> IO ()
validarArchivo ruta = do
    _ <- cargarMundo ruta
    putStrLn "Mundo válido."

-- Inicia la partida sin describir automaticamente la sala actual.
iniciarJuego :: FilePath -> IO ()
iniciarJuego ruta = do
    mundo <- cargarMundo ruta

    let (mensajes, estado) = iniciarPartida mundo
    imprimirMensajes mensajes

    unless (juegoTerminado estado) $ do
        interactivo <- hIsTerminalDevice stdin
        bucle interactivo estado

-- Lee comandos hasta ganar, salir o alcanzar el fin de la entrada.
bucle :: Bool -> Estado -> IO ()
bucle interactivo estado = do
    when interactivo $ do
        putStr "> "
        hFlush stdout

    fin <- isEOF
    unless fin $ do
        entrada <- getLine
        case leerComando entrada of
            Nothing -> do
                putStrLn "Comando no reconocido. Escribe 'ayuda' para ver los comandos."
                bucle interactivo estado
            Just comando -> do
                (mensajes, nuevoEstado) <- ejecutarComando comando estado
                imprimirMensajes mensajes

                -- Tambien detecta la victoria al tomar el objeto de la meta
                -- en la sala de destino, no solo al usar 'ir'.
                let (victoria, estadoFinal) =
                        if juegoTerminado nuevoEstado
                            then ([], nuevoEstado)
                            else verificarVictoriaInicial nuevoEstado

                imprimirMensajes victoria
                unless (juegoTerminado estadoFinal) $
                    bucle interactivo estadoFinal

-- La entrada/salida de guardar y cargar pertenece a Main/Persistencia.
ejecutarComando :: Comando -> Estado -> IO ([String], Estado)
ejecutarComando (Guardar ruta) estado = do
    resultado <- escribirArchivo ruta (serializarEstado estado)
    case resultado of
        Left detalle ->
            return (["No se pudo guardar la partida: " ++ detalle], estado)
        Right () ->
            return (["Partida guardada en " ++ ruta], estado)

ejecutarComando (Cargar ruta) estado = do
    resultado <- leerArchivo ruta
    case resultado of
        Left detalle ->
            return (["No se pudo cargar la partida: " ++ detalle], estado)
        Right texto ->
            case deserializarEstado (estadoMundo estado) texto of
                Left detalle ->
                    return (["No se pudo cargar la partida: " ++ detalle], estado)
                Right recuperado ->
                    return (["Partida cargada desde " ++ ruta], recuperado)

ejecutarComando comando estado =
    return (ejecutar comando estado)

imprimirMensajes :: [String] -> IO ()
imprimirMensajes = mapM_ putStrLn
