{-# LANGUAGE LambdaCase #-}

module Smuggler.Plugin
  ( plugin,
  )
where

import Control.Monad(unless)
import Control.Monad.IO.Class (liftIO)
import Data.List ()
import DynFlags (DynFlags, HasDynFlags (getDynFlags))
import ErrUtils
import GHC (Module, dumpDir, hsmodImports, moduleName, moduleNameString)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import HsSyn (ImportDecl (ideclImplicit))
import HscTypes (ModSummary (..))
import IOEnv (readMutVar)
import Language.Haskell.GHC.ExactPrint (setEntryDPT, transferEntryDPT, uniqueSrcSpanT, exactPrint, mergeAnns)
import Language.Haskell.GHC.ExactPrint.Transform (graftT, runTransform)
import Language.Haskell.GHC.ExactPrint.Utils (showAnnData)
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos(..))
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
import SrcLoc (GenLocated (..), unLoc)
import System.FilePath ( (</>), (-<.>) )
import System.IO (IOMode (..), withFile)
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
  let minImpFilePath = mkFilePath dflags (ms_mod modSummary)
  printMinimalImports' dflags minImpFilePath usage -- only need to do this if the dumpminimalimports flag is not set?
  tcEnv <$ liftIO (smuggling dflags minImpFilePath uses)
  where
    smuggling :: DynFlags -> FilePath -> [GlobalRdrElt] -> IO ()
    smuggling dflags minImpFilePath usage = do
      let options = parseCommandLineOptions clis

      -- 0. Read file content as a UTF-8 string (GHC accepts only ASCII or UTF-8)
      -- TODO: Use ms_hspp_buf instead, if we have it?
      setLocaleEncoding utf8

      let modulePath = ms_hspp_file modSummary
      modFileContents <- readFile modulePath
      -- parse the whole module
      runParser modulePath modFileContents >>= \case
        Left () -> error "failed to parsei module" -- pure () -- do nothing if file is invalid Haskell
        Right (anns, astMod@(L astModLoc hsMod)) -> do
          minImpFileContents <- readFile minImpFilePath

          -- parse the minimal imports file
          runParser minImpFilePath minImpFileContents >>= \case
            Left () -> do
              error "failed to parse minimal imports"
            Right (anns', astImpMod@(L astImpModLoc hsImpMod)) -> do
              --liftIO $ putStrLn $ "showAnnData\n" ++ showAnnData anns' 2 ast'

              --          let allExports = tcg_exports tcEnv
              --          let (anns'', ast'') =
              --                addExplicitExports dflags (exportAction options) allExports (anns', ast')

              --        putStrLn $ "showAnnData\n" ++ showAnnData anns'' 2 ast''

              let (astMod', (anns'', _), s) = runTransform anns $ do
                    minImports <- graftT anns' (hsmodImports hsImpMod)
                    unless (null minImports) $ setEntryDPT (head minImports) (DP (2, 0))
                    return $ L astModLoc (hsMod {hsmodImports = minImports})

              liftIO $ putStrLn $ "showAnnData\n" ++ showAnnData anns'' 2 astMod'

              let newContent = exactPrint astMod' anns''
              case newExtension options of
                Nothing -> writeFile modulePath newContent
                Just ext -> writeFile (modulePath -<.> ext) newContent

    -- This version ignores implcit imports as the result cannot be parsed
    -- back in.  (It has an '(implicit)')
    printMinimalImports' :: DynFlags -> FilePath -> [ImportDeclUsage] -> RnM ()
    printMinimalImports' dflags filename imports_w_usage =
      do
        imports' <- getMinimalImports imports_w_usage
        liftIO $
          withFile
            filename
            WriteMode
            ( \h ->
                printForUser dflags h neverQualify (vcat (map ppr (filter (not . ideclImplicit . unLoc) imports')))
            )

    -- The neverQualify is important.  We are printing Names
    -- but they are in the context of an 'import' decl, and
    -- we never qualify things inside there
    -- E.g.   import Blag( f, b )
    -- not    import Blag( Blag.f, Blag.g )!

    mkFilePath :: DynFlags -> Module -> FilePath
    mkFilePath dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise = basefn
      where
        basefn = moduleNameString (moduleName this_mod) ++ ".imports"
