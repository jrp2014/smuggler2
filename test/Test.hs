{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, mapM, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Smuggler.Options
import System.Exit
import System.FilePath
import System.IO
import System.Process.Typed
import Test.Tasty
import Test.Tasty.Golden

--

optionsList :: [Options]
optionsList =
  [ Options PreserveInstanceImports NoExportProcessing (Just "PreserveInstanceImports"),
    Options MinimiseImports NoExportProcessing (Just "MinimiseImports"),
    Options NoImportProcessing AddExplicitExports (Just "AddExplicitExports"),
    Options NoImportProcessing ReplaceExports (Just "ReplaceExports"),
    Options MinimiseImports ReplaceExports (Just "MinimiseImportsReplaceExports"),
    Options NoImportProcessing NoExportProcessing (Just "noop")
  ]

testOptions :: [Options] -> IO TestTree
testOptions opts =
  testGroup
    "All"
    <$> sequence (goldenTests <$> opts)

goldenTests :: Options -> IO TestTree
goldenTests opts = do
  testFiles <- findByExtension [".hs"] "test/tests"
  return $
    testGroup
      ("With " ++ show opts)
      [ goldenVsFileDiff
          (takeBaseName testFile) -- test name
          (\ref new -> ["diff", "-u", ref, new]) -- how to display diffs
          (replaceExtension testFile (mkExt opts)) -- golden file
          (replaceExtension testFile (fromMaybe "NoNewExtension" (newExtension opts))) -- output file
          (compile testFile opts)
        | testFile <- testFiles
      ]

main :: IO ()
main = defaultMain =<< testOptions optionsList

compile :: FilePath -> Options -> IO ()
compile testcase opts = do
  print ghcArgs
  runProcess_ ghcConfig
  where
    ghcConfig :: ProcessConfig () () ()
    ghcConfig = proc ghcCmd ghcArgs
    ghcCmd :: FilePath
    ghcCmd = "ghc"
    ghcArgs :: [String]
    ghcArgs = mkArgs opts ++ [testcase]

-- | Produce a list of command line arguments for ghc from Options
mkArgs :: Options -> [String]
mkArgs opts =
  ["-fno-code", "-fplugin=Smuggler.Plugin"]
    ++ map
      ("-fplugin-opt=Smuggler.Plugin:" ++)
      ( let p = [show (importAction opts), show (exportAction opts)]
         in case newExtension opts of
              Nothing -> p
              Just e -> e : p
      )

mkExt :: Options -> String
mkExt opts = '.' : (show (importAction opts) ++ show (exportAction opts) ++ fromMaybe "" (newExtension opts))
