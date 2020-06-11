{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (fromList, member)
import GHC.Paths (ghc)
import Smuggler2.Options (ExportAction (..), ImportAction (..), Options (..))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (lookupEnv)
import System.FilePath ((-<.>), (</>), takeBaseName, takeExtension)
import System.Process.Typed
  ( ProcessConfig,
    proc,
    runProcess_,
    setEnvInherit,
    setWorkingDirInherit,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff, writeBinaryFile)

-- | Where the tests are, relative to the project level cabal file
testDir :: FilePath
testDir = "test" </> "tests"

-- | Combinations of import and export action options to be tested
optionsList :: [Options]
optionsList =
  [ mkOptions PreserveInstanceImports NoExportProcessing [],
    mkOptions MinimiseImports ReplaceExports [],
    mkOptions MinimiseImports NoExportProcessing [],
    mkOptions NoImportProcessing AddExplicitExports [],
    mkOptions NoImportProcessing NoExportProcessing [],
    mkOptions NoImportProcessing ReplaceExports []
  ]
  where
    mkOptions :: ImportAction -> ExportAction -> [String] -> Options
    mkOptions ia ea lo= Options ia ea (Just $ mkExt ia ea) lo

-- | Make an extention for an output file
mkExt :: ImportAction -> ExportAction -> String
mkExt ia ea = show ia ++ show ea -- ++ "-" ++ takeFileName ghc

-- | Generate test for a list of 'Options' each of which specify what action to
-- take on imports and exports
testOptions :: [Options] -> IO TestTree
testOptions opts =
  testGroup "All" <$> sequenceA (goldenTests <$> opts)

-- | Generate tests for a set of 'Options' (that specifies what to do to
-- imports and exports)
goldenTests :: Options -> IO TestTree
goldenTests opts = do
  testFiles <- findByExtension' [".hs"] testDir
  return $
    testGroup
      testName
      [ goldenVsFileDiff
          (takeBaseName testFile) -- test name
          -- The -G. is needed because cabal sdist changes the golden file
          -- permissions and so all the tests fail.
          (\ref new -> ["git", "diff", "--no-index", "-G.", ref, new]) -- how to display diffs
          (testFile -<.> testName ++ "-golden") -- golden file
          outputFilename
          ( do
              -- Write a default output file for those tests where smuggler
              -- (deliberately) does not generate a new one
              writeBinaryFile outputFilename "Source file was not touched\r\n"
              compile testFile opts
          )
        | testFile <- sort testFiles,
          let outputFilename = testFile -<.> testName
      ]
  where
    testName = fromMaybe "NoNewExtension" (newExtension opts)

-- | A version of 'Test.Tasty.Golden.findByExtension' that does not look into
-- subdirectories.  This allows tests to be run on a module that imports other
-- modules without risking the race condition where smuggler is being run
-- on the imported module directly, and also when the importing module is being
-- tested. ('Tasty' runs test in parallel and the same output files are
-- produced in both direct and imported cases.) Putting the imported module in
-- a subdirectory and not testing it direcly avoids this race condition.
findByExtension' ::
  -- | extensions
  [FilePath] ->
  -- | directory
  FilePath ->
  -- | paths
  IO [FilePath]
findByExtension' extsList = go
  where
    exts = Set.fromList extsList
    go :: FilePath -> IO [FilePath]
    go dir = do
      allEntries <- getDirectoryContents dir
      let entries = filter (not . (`elem` [".", ".."])) allEntries
      fmap concat $ forM entries $ \e -> do
        let path = dir ++ "/" ++ e
        isDir <-
          doesDirectoryExist
            path
        return $
          if isDir
            then [] -- don't recurse
            else [path | takeExtension path `Set.member` exts]

-- | Just run all the tests
main :: IO ()
main = defaultMain =<< testOptions optionsList

-- | Use `cabal exec` to run the compilation, so that the smuggler plugin is
-- picked up from the local database.  GHC alone would use the global one.
compile :: FilePath -> Options -> IO ()
compile testcase opts = do
  cabalPath <- lookupEnv "CABAL" -- find, eg, @/opt/ghc/bin/cabal@ or @cabal -vnormal+nowrap@
  let cabalCmd = words $ fromMaybe "cabal" cabalPath -- default to @cabal@ if @CABAL@ is not set
  let cabalConfig =
        setWorkingDirInherit . setEnvInherit $
          proc
            (head cabalCmd)
            (tail cabalCmd ++ cabalArgs) :: ProcessConfig () () ()
  runProcess_ cabalConfig
  where
    cabalArgs :: [String]
    cabalArgs =
      -- - not sure why it is necessary to mention the smuggler2 package explicitly,
      --   but it appears to be hidden otherwise.
      -- - This also puts the .imports files that smuggler generates somewhere they
      --   can easily be found
      [ "--with-compiler=" ++ ghc,
        "exec",
        ghc,
        "--",
        "-package smuggler2",
        "-v0",
        "-dumpdir=" ++ testDir,
        "-fno-code",
        "-i" ++ testDir,
        "-fplugin=Smuggler2.Plugin"
      ]
        ++ map
          ("-fplugin-opt=Smuggler2.Plugin:" ++)
          ( let ia = importAction opts
                ea = exportAction opts
                -- the extension should have been set by 'optionsList'
             in (fromMaybe "missng" (newExtension opts) : [show ia, show ea])
          )
        ++ [testcase]
