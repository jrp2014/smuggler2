{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (fromList, member)
import GHC (mkModuleName, moduleNameString)
import GHC.Paths (ghc)
import Smuggler2.Options (ExportAction (..), ImportAction (..), Options (..))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Exit
import System.FilePath ((-<.>), (</>), takeBaseName, takeExtension)
import System.Process
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff, writeBinaryFile)

-- | Where the tests are, relative to the project level cabal file
testDir :: FilePath
testDir = "test" </> "tests"

-- | Combinations of import and export action options to be tested
optionsList :: [Options]
optionsList =
  [ mkOptions PreserveInstanceImports NoExportProcessing [] ["Prelude"],
    mkOptions MinimiseImports ReplaceExports [] [],
    mkOptions MinimiseImports NoExportProcessing ["Prelude", "Data.Version"] [],
    mkOptions NoImportProcessing AddExplicitExports [] [],
    mkOptions NoImportProcessing NoExportProcessing [] [],
    mkOptions NoImportProcessing ReplaceExports [] []
  ]
  where
    mkOptions :: ImportAction -> ExportAction -> [String] -> [String] -> Options
    mkOptions ia ea lo mo =
      Options
        ia
        ea
        (Just $ mkExt ia ea lo mo)
        (mkModuleName <$> lo)
        (mkModuleName <$> mo)

-- | Make an extention for an output file
mkExt :: ImportAction -> ExportAction -> [String] -> [String] -> String
mkExt ia ea lo mo =
  show ia
    ++ show ea
    ++ filter (/= '.') (concat lo ++ concat mo) -- ++ "-" ++ takeFileName ghc

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
          ( \ref new -> -- how to display diffs
          -- The -G. is needed because cabal sdist changes the golden file
          -- permissions and so all the tests fail.
              [ "git",
                "diff",
                "-G.",
                "--ignore-cr-at-eol",
                "--ws-error-highlight=all",
                "--no-index",
                "--exit-code",
                ref,
                new
              ]
          )
          (testFile -<.> testName ++ "-golden") -- golden file
          outputFilename
          ( do
              -- Write a default output file for those tests where smuggler2
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
-- modules without risking the race condition where smuggler2 is being run
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

-- | Run a compilation. Assumes that @smuggler2@ has been built with
-- @--write-ghc-environment-files=always@ so that it is picked up from
-- the local database.
compile :: FilePath -> Options -> IO ()
compile testcase opts = do
  (_, _, _, pid) <- createProcess (proc ghc ghcArgs) {  use_process_jobs = True }
  r <- waitForProcess pid
  return $ case r of
    ExitSuccess -> ()
    ExitFailure c -> error $ "Failed to compile " ++ testcase ++ ". Exit code " ++ show c

  where
    ghcArgs :: [String]
    ghcArgs =
      [ "-v0",
        "-dumpdir=" ++ testDir,
        "-fno-code",
        "-i" ++ testDir,
        "-fplugin=Smuggler2.Plugin"
      ]
        ++ map
          ("-fplugin-opt=Smuggler2.Plugin:" ++)
          ( let ia = importAction opts
                ea = exportAction opts
                ne = newExtension opts
                lo = intercalate "," (moduleNameString <$> leaveOpenImports opts)
                mo = intercalate "," (moduleNameString <$> makeOpenImports opts)
             in -- The extension should have been set by 'optionsList'
                -- The rest of the list are the other arguments for the test
                ( fromMaybe "missng" ne : [show ia, show ea]
                    ++  ["LeaveOpenImports:" ++ lo | not (null lo)]
                    ++  ["MakeOpenImports:" ++ mo | not (null mo)]
                )
          )
        ++ [testcase]
