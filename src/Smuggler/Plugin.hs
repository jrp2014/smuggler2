{-# LANGUAGE LambdaCase #-}

module Smuggler.Plugin
  ( plugin,
  )
where


import Control.Monad ( unless, when )
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
    ( graftT, runTransform, runTransformFrom )
import Language.Haskell.GHC.ExactPrint.Types ( DeltaPos(..) )
import Language.Haskell.GHC.ExactPrint.Utils ()
import Outputable
    ( Outputable(ppr), neverQualify, printForUser, vcat )
import Plugins
    ( CommandLineOption,
      Plugin(..),
      purePlugin,
      defaultPlugin )
import RnNames
    ( ImportDeclUsage, findImportUsage, getMinimalImports )
import Smuggler.Export ( addExplicitExports )
import Smuggler.Import ( replaceImports )
import Smuggler.Options
    ( parseCommandLineOptions,
      ImportAction(..), ExportAction(..),
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
      pluginRecompile = purePlugin -- ^ Don't force recompilation.  [Is this the right approach?]
    }

-- | 
noop :: [CommandLineOption] -> Bool
noop clopts = (importAction options == NoImportProcessing) && (exportAction options == NoExportProcessing)
  where
    options = parseCommandLineOptions clopts
smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clopts modSummary tcEnv = do
  when (noop clopts) (pure ())

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
        Left () -> pure () -- do nothing if file is invalid Haskell
        Right (annsHsMod, astHsMod@(L astHsModLoc hsMod)) -> do
          minImpFileContents <- readFile minImpFilePath

          -- TODO:: skip if no procesing option is selected
          -- parse the minimal imports file
          runParser dflags minImpFilePath minImpFileContents >>= \case
            Left () -> pure ()
            Right (annsImpMod, L _ impMod) -> do

              let (astHsMod', (annsHsMod', locIndex), ilog) = runTransform annsHsMod $ do
                  {-
                    minImports <- graftT annsImpMod (hsmodImports impMod)
                    -- nudge down the imports list onto a new line
                    unless (null minImports) $ setEntryDPT (head minImports) (DP (2, 0))
                    return $ L astHsModLoc (hsMod {hsmodImports = minImports})

-}
                    replaceImports (importAction options) annsImpMod (hsmodImports impMod) astHsMod

              liftIO $ putStrLn $ "Imports transformed " ++ unlines ilog

              -- liftIO $ putStrLn $ "showAnnData\n" ++ showAnnData anns'' 2 astMod'
  
              let allExports = tcg_exports tcEnv
              let (astHsMod'', (annsHsMod'', _), elog) = runTransformFrom locIndex annsHsMod' $
                              addExplicitExports (exportAction options) allExports astHsMod'

              liftIO $ putStrLn $ "Exoprts transformed " ++ unlines elog

              let newContent = exactPrint astHsMod'' annsHsMod''
              case newExtension options of
                Nothing -> writeFile modulePath newContent
                Just ext -> writeFile (modulePath -<.> ext) newContent
 
    -- 
    options :: Options
    options = parseCommandLineOptions clopts

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
