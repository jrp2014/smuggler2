{-# LANGUAGE LambdaCase #-}

module Smuggler.Plugin
  ( plugin,
  )
where


import Control.Monad ( unless )
import Control.Monad.IO.Class ( liftIO )
import DynFlags ( dumpDir, DynFlags, HasDynFlags(getDynFlags) )
import GHC
    ( moduleNameString,
      HsModule(hsmodImports),
      Module(moduleName) , ImportDecl(..))
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import HscTypes ( ModSummary(..) )
import IOEnv ( readMutVar )
import Language.Haskell.GHC.ExactPrint ( exactPrint, setEntryDPT )
import Language.Haskell.GHC.ExactPrint.Transform
    ( graftT, runTransform )
import Language.Haskell.GHC.ExactPrint.Types ( DeltaPos(..) )
import Language.Haskell.GHC.ExactPrint.Utils ()
import Outputable
    ( Outputable(ppr), neverQualify, printForUser, vcat )
import Plugins
    ( CommandLineOption,
      Plugin(..),
      PluginRecompile(..),
      defaultPlugin )
import RnNames
    ( ImportDeclUsage, findImportUsage, getMinimalImports )
import Smuggler.Export ( addExplicitExports )
import Smuggler.Options
    ( parseCommandLineOptions,
      ImportAction(MinimiseImports),
      Options(exportAction, newExtension, importAction) )
import Smuggler.Parser ( runParser )
import SrcLoc ( GenLocated(..), unLoc )
import System.FilePath ( (-<.>), (</>) )
import System.IO ( IOMode(..), withFile )
import TcRnTypes ( RnM, TcGblEnv(..), TcM )


plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = smugglerPlugin,
      pluginRecompile = smugglerRecompile
    }

-- TODO: would it be worth computing a fingerprint to force recompile if
-- imports were removed? Or don't recompile if CommandLineOptions indicate a
-- noop
smugglerRecompile :: [CommandLineOption] -> IO PluginRecompile
smugglerRecompile _ = return NoForceRecompile

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clis modSummary tcEnv = do
  -- TODO:: Used only for debugging (showSDoc dflags (ppr _ ))
  dflags <- getDynFlags

  -- TODO: skip all this if nothing is to be done
  uses <- readMutVar $ tcg_used_gres tcEnv
  let imports = tcg_rn_imports tcEnv
  let usage = findImportUsage imports uses
  let minImpFilePath = mkFilePath dflags (ms_mod modSummary)
  printMinimalImports' dflags minImpFilePath usage
  tcEnv <$ liftIO (smuggling dflags minImpFilePath)
  where
    smuggling :: DynFlags -> FilePath -> IO ()
    smuggling dflags minImpFilePath = do
      -- 0. Read file content as a UTF-8 string (GHC accepts only ASCII or UTF-8)
      -- TODO: Use ms_hspp_buf instead, if we have it?
      setLocaleEncoding utf8

      let modulePath = ms_hspp_file modSummary

      modFileContents <- readFile modulePath
      -- parse the whole module
      runParser dflags modulePath modFileContents >>= \case
        Left () -> error "failed to parsei module" -- pure () -- do nothing if file is invalid Haskell
        Right (anns, L astModLoc hsMod) -> do
          minImpFileContents <- readFile minImpFilePath

          -- TODO:: skip if no procesing option is selected
          -- parse the minimal imports file
          runParser dflags minImpFilePath minImpFileContents >>= \case
            Left () -> do
              error "failed to parse minimal imports" -- pure ()
            Right (anns', L _ hsImpMod) -> do

              -- Is this grafting necessary?
              let (astMod', (anns'', _), _) = runTransform anns $ do
                    minImports <- graftT anns' (hsmodImports hsImpMod)
                    -- nudge down the imports list onto a new line
                    unless (null minImports) $ setEntryDPT (head minImports) (DP (2, 0))
                    return $ L astModLoc (hsMod {hsmodImports = minImports})

              -- liftIO $ putStrLn $ "showAnnData\n" ++ showAnnData anns'' 2 astMod'
  
              let allExports = tcg_exports tcEnv
              let (anns''', astMod'') =
                              addExplicitExports dflags (exportAction options) allExports (anns'', astMod')

              let newContent = exactPrint astMod'' anns'''
              case newExtension options of
                Nothing -> writeFile modulePath newContent
                Just ext -> writeFile (modulePath -<.> ext) newContent
 
    -- 
    options :: Options
    options = parseCommandLineOptions clis

    -- This version of the GHC function ignores implicit imports, as the result cannot be parsed
    -- back in.  (There is an extraneous (implicit)')
    -- It also provides for leaving out instance-only imports (eg, Data.List() )
    printMinimalImports' :: DynFlags -> FilePath -> [ImportDeclUsage] -> RnM ()
    printMinimalImports' dflags filename imports_w_usage =
      do
        imports' <- getMinimalImports imports_w_usage
        liftIO $
          withFile
            filename
            WriteMode
            ( \h ->
                -- The neverQualify is important.  We are printing Names
                -- but they are in the context of an 'import' decl, and
                -- we never qualify things inside there
                -- E.g.   import Blag( f, b )
                -- not    import Blag( Blag.f, Blag.g )!
                printForUser dflags h neverQualify (vcat (map ppr (filter (letThrough . unLoc) imports')))
            )
      where
        notImplicit :: ImportDecl pass -> Bool
        notImplicit = not . ideclImplicit
        
        notInstancesOnly :: ImportDecl pass -> Bool
        notInstancesOnly i = case ideclHiding i of
          Just (False, L _ []) -> False
          _ -> True

        keepInstanceOnlyImports :: Bool
        keepInstanceOnlyImports = importAction options /= MinimiseImports
        
        letThrough :: ImportDecl pass -> Bool
        letThrough i = notImplicit i && (keepInstanceOnlyImports || notInstancesOnly i)
   
    mkFilePath :: DynFlags -> Module -> FilePath
    mkFilePath dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise = basefn
      where
        basefn = "smuggler-" ++ moduleNameString (moduleName this_mod) ++ ".imports"
