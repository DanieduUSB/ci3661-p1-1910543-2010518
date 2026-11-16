module Main (main) where

import Test.HUnit
import System.Exit (exitFailure)

import Comandos (testsComandos)
import Nucleo (testsNucleo)
import Reconocedor (testsReconocedor)
import Mundo (testsMundo)


main :: IO()
main = do
    resultado <- runTestTT $ TestList
        [ TestLabel "Comandos"      testsComandos
        , TestLabel "Nucleo"        testsNucleo
        , TestLabel "Reconocedor"   testsReconocedor
        , TestLabel "Mundo"         testsMundo
        ]
    
    if errors resultado + failures resultado > 0
        then exitFailure
        else return ()