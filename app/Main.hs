module Main where

import GHC.Paths (ghc)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process.Typed
  ( proc,
    runProcess,
    setEnvInherit,
    setWorkingDirInherit,
  )

main :: IO ()
main = do
  args <- getArgs

  runProcess
    ( setWorkingDirInherit . setEnvInherit $
        proc
          ghc
          ( "-package-env=default" :
            "-package smuggler2" :
            "-fplugin=Smuggler2.Plugin" :
            args
          )
    )
    >>= exitWith
