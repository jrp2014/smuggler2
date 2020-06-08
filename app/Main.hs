module Main where

import GHC.Paths
import Smuggler2.Plugin
import System.Environment
import System.Exit
import System.Process.Typed

main :: IO ()
main = do
  args <- getArgs
  runProcess
    ( setWorkingDirInherit . setEnvInherit $
        shell
          ( ghc
              ++ "-fplugin=Smuggler2,Plugin "
              ++ unwords args
          )
    )
    >>= exitWith
