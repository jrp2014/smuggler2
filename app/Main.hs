module Main where

import GHC.Paths
import System.Environment
import System.Exit
import System.Process.Typed
import Smuggler2.Plugin

main :: IO ()
main = do
  args <- getArgs
  runProcess
    ( proc
        ghc
        ( "-v"
            : "-dynamic"
            : "-user-package-db"
            : "-package-key smuggler2-0.3.3.2"
            : "-package smuggler2"
            : "-fplugin=Smuggler2,Plugin"
            : args
        )
    )
    >>= exitWith
