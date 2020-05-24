{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import GHC.Paths (ghc)
import Smuggler.Options
  ( ExportAction (..),
    ImportAction (..),
    Options (..),
  )
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((-<.>), (</>), takeBaseName)
import System.Process.Typed (ProcessConfig, proc, runProcess_)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFileDiff)

-- | Where the tests are, relative to the project level cabal file
testDir :: FilePath
testDir = "test" </> "tests"

-- | Combinations of import and export action options to be tested
optionsList :: [Options]
optionsList =
  [ mkOptions PreserveInstanceImports NoExportProcessing,
    mkOptions MinimiseImports ReplaceExports,
    mkOptions MinimiseImports NoExportProcessing,
    mkOptions NoImportProcessing AddExplicitExports,
    mkOptions NoImportProcessing NoExportProcessing,
    mkOptions NoImportProcessing ReplaceExports
  ]
  where
    mkOptions :: ImportAction -> ExportAction -> Options
    mkOptions ia ea = Options ia ea (Just $ mkExt ia ea)

mkExt :: ImportAction -> ExportAction -> String
mkExt ia ea = show ia ++ show ea

testOptions :: [Options] -> IO TestTree
testOptions opts =
  testGroup "All" <$> sequenceA (goldenTests <$> opts)

goldenTests :: Options -> IO TestTree
goldenTests opts = do
  testFiles <- findByExtension [".hs"] testDir
  return $
    testGroup
      testName
      [ goldenVsFileDiff
          (takeBaseName testFile) -- test name
          (\ref new -> ["diff", "-u", ref, new]) -- how to display diffs
          (testFile -<.> testName ++ "-golden") -- golden file
          (testFile -<.> testName) -- output file
          (compile testFile opts)
        | testFile <- testFiles
      ]
  where
    testName = fromMaybe "NoNewExtension" (newExtension opts)

main :: IO ()
main = defaultMain =<< testOptions optionsList

-- | Use `cabal exec` to run the compilation, so that the smuggler plugin is
-- picked up from the local database.  GHC alone would use the global one.
compile :: FilePath -> Options -> IO ()
compile testcase opts = do
    env <- getEnvironment
    print env
    cabalPath <- lookupEnv "CABAL" -- find, eg, @/opt/ghc/bin/cabal@ or @cabal -vnormal+nowrap@
    let cabalCmd = words $ fromMaybe "cabal" cabalPath -- default to "cabal" if @CABAL@ is not set
    let cabalConfig = proc (head cabalCmd) (tail cabalCmd ++ cabalArgs) :: ProcessConfig () () ()
    print cabalConfig
    runProcess_ cabalConfig
  where
    cabalArgs :: [String]
    cabalArgs =
      -- - not sure why it is necessary to mention the smuggler package explicitly,
      --   but it appears to be hidden otherwise.
      -- - This puts the .imports files that smuggler generates somewhere they
      --   can easily be found
      ["exec", ghc, "--", "-package smuggler", "-v0", "-dumpdir=" ++ testDir, "-fno-code", "-fplugin=Smuggler.Plugin"]
        ++ map
          ("-fplugin-opt=Smuggler.Plugin:" ++)
          ( let ia = importAction opts
                ea = exportAction opts
                p = [show ia, show ea]
             in case newExtension opts of
                  Nothing -> mkExt ia ea : p -- provide a default extension
                  Just e -> e : p
          )
        ++ [testcase]
