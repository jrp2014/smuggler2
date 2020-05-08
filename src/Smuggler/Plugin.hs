{-# LANGUAGE LambdaCase #-}

module Smuggler.Plugin
  ( plugin,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List ()
import DynFlags (DynFlags, HasDynFlags (getDynFlags))
import ErrUtils
import GHC (Module, dumpDir, moduleName, moduleNameString)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import HsSyn (ImportDecl (ideclImplicit))
import HscTypes (ModSummary (..))
import IOEnv (readMutVar)
import Language.Haskell.GHC.ExactPrint (exactPrint)
import Language.Haskell.GHC.ExactPrint.Utils (showAnnData)
import Outputable
import Plugins
  ( CommandLineOption,
    Plugin (..),
    PluginRecompile (..),
    defaultPlugin,
  )
import RdrName (GlobalRdrElt)
import RnNames (ImportDeclUsage, findImportUsage, getMinimalImports, printMinimalImports)
import Smuggler.Export (addExplicitExports)
import Smuggler.Import (minimiseImports)
import Smuggler.Options (Options (..), parseCommandLineOptions)
import Smuggler.Parser (runImportsParser, runParser)
import SrcLoc (unLoc)
import System.FilePath ((</>))
import System.FilePath ((-<.>))
import System.IO (IOMode (..), openFile)
import TcRnTypes (RnM, TcGblEnv (..), TcM)

plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = smugglerPlugin,
      pluginRecompile = smugglerRecompile
    }

-- TODO: would it be worth computing a fingerprint to force recompile if
-- imports were removed?
smugglerRecompile :: [CommandLineOption] -> IO PluginRecompile
smugglerRecompile _ = return NoForceRecompile

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clis modSummary tcEnv = do
  -- TODO:: Used only for debugging (showSDoc dflags (ppr _ ))
  dflags <- getDynFlags

  --let modulePath = ms_hspp_file modSummary
  uses <- readMutVar $ tcg_used_gres tcEnv
  let imports = tcg_rn_imports tcEnv
  let usage = findImportUsage imports uses
  printMinimalImports' dflags (ms_mod modSummary) usage -- only need to do this if the dumpminimalimports flag is not set?
  tcEnv <$ liftIO (smuggling dflags uses)
  where
    -- This version ignores implcit imports as the result cannot be parsed
    -- back in.  (It has an '(implicit)')
    printMinimalImports' :: DynFlags -> Module -> [ImportDeclUsage] -> RnM ()
    printMinimalImports' dflags this_mod imports_w_usage =
      do
        imports' <- getMinimalImports imports_w_usage
        liftIO $
          do
            h <- openFile (mkFilename dflags this_mod) WriteMode
            printForUser dflags h neverQualify (vcat (map ppr (filter (not . ideclImplicit . unLoc) imports')))
        -- The neverQualify is important.  We are printing Names
        -- but they are in the context of an 'import' decl, and
        -- we never qualify things inside there
        -- E.g.   import Blag( f, b )
        -- not    import Blag( Blag.f, Blag.g )!

    mkFilename :: DynFlags -> Module -> FilePath
    mkFilename dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise = basefn
      where
        basefn = moduleNameString (moduleName this_mod) ++ ".imports"

    smuggling :: DynFlags -> [GlobalRdrElt] -> IO ()
    smuggling dflags uses = do
      let options = parseCommandLineOptions clis

      -- 0. Read file content as a UTF-8 string (GHC accepts only ASCII or UTF-8)
      -- TODO: Use ms_hspp_buf instead, if we have it?
      setLocaleEncoding utf8

      let modulePath = ms_hspp_file modSummary
      fileContents <- readFile modulePath
      -- parse the whole module
      runParser modulePath fileContents >>= \case
        Left () -> error "failed to parsei module" -- pure () -- do nothing if file is invalid Haskell
        Right (anns, ast) -> do
          let minimalImportsFilename = mkFilename dflags (ms_mod modSummary)
          fileContents <- readFile minimalImportsFilename

          -- parse the minimal imports file
          runParser minimalImportsFilename fileContents >>= \case
            Left () -> do
              error "failed to parse minimal imports"
            Right (anns', ast') -> do
              liftIO $ putStrLn $ "showAnnData\n" ++ showAnnData anns' 2 ast'

              --          let allExports = tcg_exports tcEnv
              --          let (anns'', ast'') =
              --                addExplicitExports dflags (exportAction options) allExports (anns', ast')

              --        putStrLn $ "showAnnData\n" ++ showAnnData anns'' 2 ast''

              -- 4. Output the result
              let options = parseCommandLineOptions clis
              let modulePath = ms_hspp_file modSummary
              let newContent = exactPrint ast anns'
              case newExtension options of
                Nothing -> writeFile modulePath newContent
                Just ext -> writeFile (modulePath -<.> ext) newContent
