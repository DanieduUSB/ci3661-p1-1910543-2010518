module Main (main) where

import Test.HUnit
import System.Exit (exitFailure)

import Comandos (testsComandos)


main :: IO()
main = do
    resultado <- runTestTT $ TestList
        [ TestLabel "Comandos" testsComandos
        ]
    
    if errors resultado + failures resultado > 0
        then exitFailure
        else return ()