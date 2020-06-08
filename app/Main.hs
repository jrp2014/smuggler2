module Main (
  main ) where

import GHC.Paths ( ghc )
import System.Environment ( getArgs )
import System.Exit ( exitWith )
import System.Process.Typed
    ( runProcess, setEnvInherit, setWorkingDirInherit, shell )

main :: IO ()
main = do
  args <- getArgs
  runProcess
    ( setWorkingDirInherit . setEnvInherit $
        shell
          ( ghc
              ++ " -fplugin=Smuggler2.Plugin "
              ++ unwords args
          )
    )
    >>= exitWith
