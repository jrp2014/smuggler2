{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Smuggler.Options
  ( ExportAction (..),
    ImportAction (..),
    Options (..),
  )
import System.FilePath ((-<.>), takeBaseName)
import System.Process.Typed (ProcessConfig, proc, runProcess_)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFileDiff)

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
  testGroup "All" <$> sequenceA (goldenTests <$> opts)

goldenTests :: Options -> IO TestTree
goldenTests opts = do
  testFiles <- findByExtension [".hs"] "test/tests"
  return $
    testGroup
      (fromMaybe "NoNewExtension" (newExtension opts))
      [ goldenVsFileDiff
          (takeBaseName testFile) -- test name
          (\ref new -> ["git", "diff", ref, new]) -- how to display diffs
            -- (\ref new -> ["diff", "-u", ref, new]) -- how to display diffs
          (testFile -<.> fromMaybe "golden" (newExtension opts) ++ "-golden") -- golden file
          (testFile -<.> fromMaybe "NoNewExtension" (newExtension opts)) -- output file
          (compile testFile opts)
        | testFile <- testFiles
      ]

main :: IO ()
main = defaultMain =<< testOptions optionsList

-- | Use `cabal exec` to run the compilation, so that the smuggler plugin is
-- picked up from the local database.  GHC alone would use the global one.
compile :: FilePath -> Options -> IO ()
compile testcase opts = runProcess_ cabalConfig
  where
    cabalConfig :: ProcessConfig () () ()
    cabalConfig = proc cabalCmd cabalArgs

    cabalCmd :: FilePath
    cabalCmd = "cabal"

    cabalArgs :: [String]
    cabalArgs =
      -- no sure why it is necessary to mention the smuggler package explictly,
      -- but it appears to be hidden otherwise
      ["exec", "--", "ghc", "-package smuggler", "-fno-code", "-fplugin=Smuggler.Plugin"]
        ++ map
          ("-fplugin-opt=Smuggler.Plugin:" ++)
          ( let p = [show (importAction opts), show (exportAction opts)]
             in case newExtension opts of
                  Nothing -> p
                  Just e -> e : p
          )
        ++ [testcase]
