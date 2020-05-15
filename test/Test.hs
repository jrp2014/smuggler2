{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe ( fromMaybe )
import Smuggler.Options
    ( ExportAction(..), ImportAction(..), Options(..) )
import System.FilePath ( replaceExtension, takeBaseName )
import System.Process.Typed ( proc, runProcess_, ProcessConfig )
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.Golden ( findByExtension, goldenVsFileDiff )

-- | Combinations of import and export action options to be tested
optionsList :: [Options]
optionsList =
  [ mkOptions PreserveInstanceImports NoExportProcessing,
    mkOptions MinimiseImports NoExportProcessing,
    mkOptions NoImportProcessing AddExplicitExports,
    mkOptions NoImportProcessing ReplaceExports,
    mkOptions MinimiseImports ReplaceExports,
    mkOptions NoImportProcessing NoExportProcessing
  ]
  where
    mkOptions :: ImportAction -> ExportAction -> Options
    mkOptions i e = Options i e (Just (show i ++ show e))

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
      (fromMaybe "NoNewExtension" (newExtension opts))
      [ goldenVsFileDiff
          (takeBaseName testFile) -- test name
          (\ref new -> ["diff", "-u", ref, new]) -- how to display diffs
          (replaceExtension testFile (fromMaybe "golden" (newExtension opts)) ++ "-golden") -- golden file
          (replaceExtension testFile (fromMaybe "NoNewExtension" (newExtension opts))) -- output file
          (compile testFile opts)
        | testFile <- testFiles
      ]

main :: IO ()
main = defaultMain =<< testOptions optionsList

-- | Use `cabal exec` to run the compilation, so that the smuggler plugin is
-- picked up from the local database.  GHC alone uses the global one.
compile :: FilePath -> Options -> IO ()
compile testcase opts = do
  runProcess_ cabalConfig
  where
    cabalConfig :: ProcessConfig () () ()
    cabalConfig = proc cabalCmd cabalArgs
    cabalCmd :: FilePath
    cabalCmd = "cabal"
    cabalArgs :: [String]
    cabalArgs = mkArgs opts ++ [testcase]

-- | Produce a list of command line arguments for ghc from Options
mkArgs :: Options -> [String]
mkArgs opts =
  ["exec", "--", "ghc", "-fno-code", "-package smuggler", "-fplugin=Smuggler.Plugin"]
    ++ map
      ("-fplugin-opt=Smuggler.Plugin:" ++)
      ( let p = [show (importAction opts), show (exportAction opts)]
         in case newExtension opts of
              Nothing -> p
              Just e -> e : p
      )
