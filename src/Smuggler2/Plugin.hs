{-# LANGUAGE LambdaCase #-}

module Smuggler2.Plugin
  ( plugin,
  )
where

import Avail (AvailInfo, Avails)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isNothing)
import Data.Version (showVersion)
import DynFlags (DynFlags (dumpDir), HasDynFlags (getDynFlags))
import ErrUtils (compilationProgressMsg, fatalErrorMsg)
import GHC
  ( GenLocated (L),
    GhcPs,
    HsModule (hsmodExports, hsmodImports),
    ImportDecl (ideclHiding, ideclImplicit),
    LIE,
    LImportDecl,
    Located,
    ModSummary (ms_hspp_buf, ms_hspp_file, ms_mod),
    Module (moduleName),
    ParsedSource,
    moduleNameString,
    unLoc,
  )
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import IOEnv (MonadIO (liftIO), readMutVar)
import Language.Haskell.GHC.ExactPrint
  ( Anns,
    TransformT,
    addTrailingCommaT,
    exactPrint,
    graftT,
    runTransform,
    setEntryDPT,
  )
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (DP))
import Outputable (Outputable (ppr),  neverQualify, printForUser, text, vcat)
import Paths_smuggler2 (version)
import Plugins
  ( CommandLineOption,
    Plugin (pluginRecompile, typeCheckResultAction),
    defaultPlugin,
    purePlugin,
  )
import RnNames (ImportDeclUsage, findImportUsage, getMinimalImports)
import Smuggler2.Anns (mkExportAnnT, mkLoc, mkParenT)
import Smuggler2.Options
  ( ExportAction (AddExplicitExports, NoExportProcessing, ReplaceExports),
    ImportAction (MinimiseImports, NoImportProcessing),
    Options (exportAction, importAction, newExtension),
    parseCommandLineOptions,
  )
import Smuggler2.Parser (runParser)
import StringBuffer (StringBuffer (StringBuffer), lexemeToString)
import System.Directory (removeFile)
import System.FilePath ((-<.>), (</>))
import System.IO (IOMode (WriteMode), withFile)
import TcRnExports (exports_from_avail)
import TcRnTypes
  ( RnM,
    TcGblEnv (tcg_exports, tcg_imports, tcg_mod, tcg_rdr_env, tcg_rn_exports, tcg_rn_imports, tcg_used_gres),
    TcM,
  )

plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = smugglerPlugin,
      pluginRecompile = purePlugin -- Don't force recompilation.  [Is this the right approach?]
    }

-- | The plugin itself
smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clopts modSummary tcEnv
  -- short circuit, if nothing to do
  | (importAction options == NoImportProcessing)
      && (exportAction options == NoExportProcessing) =
    return tcEnv
  | otherwise = do
    -- Get the imports and their usage
    let imports = tcg_rn_imports tcEnv
    uses <- readMutVar $ tcg_used_gres tcEnv
    let usage = findImportUsage imports uses

    -- This ensures that the source file is not touched if there are no unused
    -- imports, or exports already exist and we are not replacing them
    let noUnusedImports = all (\(_decl, _used, unused) -> null unused) usage
    let hasExplicitExports = case tcg_rn_exports tcEnv of
          Nothing -> False -- There is not even a module header
          (Just []) -> False
          (Just _) -> True
    -- ... so short circuit if:
    -- - we are skipping import processing or there are no unused imports, and
    -- - we are skipping export processing or there are explict exports and we are not replacing them
    -- (This is not a complete check; ideally, that the new imp/exports are
    -- different from the existing ones, etc)
    if (importAction options == NoImportProcessing || noUnusedImports) &&
       (     exportAction options == NoExportProcessing
         || (hasExplicitExports && exportAction options /= ReplaceExports)
       )
      then return tcEnv
      else do
        dflags <- getDynFlags
        liftIO $ compilationProgressMsg dflags ("smuggler: " ++ showVersion version)

        -- Dump GHC's view of what the minimal imports are for the current
        -- module, so that they can be annotated when parsed back in
        -- This is needed because 'getMinimalImports` returns a list of import
        -- declarations that are `GhcRn` but `ghc-exactPrint` operates in
        -- `GhcPs`
        let minImpFilePath = mkMinimalImportsPath dflags (ms_mod modSummary)
        printMinimalImports' dflags minImpFilePath usage

        -- Run smuggling only for its side effects
        tcEnv <$ smuggling dflags minImpFilePath

  where

    -- Does all the work
    smuggling :: DynFlags -> FilePath -> RnM ()
    smuggling dflags minImpFilePath = do

      let modulePath = ms_hspp_file modSummary

      -- Read files as UTF-8 strings (GHC accepts only ASCII or UTF-8)
      liftIO $ setLocaleEncoding utf8

      -- Get the pre-processed module source code
      let modFileContents = case ms_hspp_buf modSummary of
            -- Not clear under what circumstances this could happen
            Nothing -> error $ "smuggler: missing source file: " ++ modulePath
            Just contents -> strBufToStr contents

      -- Parse the whole module
      runParser dflags modulePath modFileContents >>= \case
        Left () -> return () -- do nothing if file is invalid Haskell
        Right (annsHsMod, astHsMod) -> do
          -- Read the dumped file of minimal imports
          minImpFileContents <- liftIO $ readFile minImpFilePath

          -- Parse the minimal imports file -- gets the annnotations too
          runParser dflags minImpFilePath minImpFileContents >>= \case
            Left () ->
              liftIO $
                fatalErrorMsg dflags (text $ "smuggler: failed to parse minimal imports from " ++ minImpFilePath)
            Right (annsImpMod, L _ impMod) -> do

              -- The actual minimal imports themselves, as generated by GHC
              let minImports = hsmodImports impMod

              -- What is exported by the module
              exports <-
                if exportAction options == ReplaceExports
                  then exportable
                  else return $ tcg_exports tcEnv -- what is currently exported

              -- Bringing it all together: generate a new ast and annotations for it
              let (astHsMod', (annsHsMod', _locIndex), _log) =
                    runTransform annsHsMod $
                      replaceImports annsImpMod minImports astHsMod
                        >>= addExplicitExports exports

              -- Print the result
              let newContent = exactPrint astHsMod' annsHsMod'
              liftIO $ case newExtension options of
                Nothing -> writeFile modulePath newContent
                Just ext -> writeFile (modulePath -<.> ext) newContent

            -- Clean up: delete the GHC-generated imports file
              liftIO $ removeFile minImpFilePath

          where

            -- Generates the things that would be exportabe if there were no
            -- explict export header, so suitable for replacing one
            exportable :: RnM [AvailInfo]
            exportable = do
              let rdr_env = tcg_rdr_env tcEnv
              let imports = tcg_imports tcEnv --  actually not needed for the Nothing case
              let this_mod = tcg_mod tcEnv
              exports <- exports_from_avail Nothing rdr_env imports this_mod
              return (snd exports)

            --  | Replace a target module's imports
            --  See <https://github.com/facebookincubator/retrie/blob/master/Retrie/CPP.hs>
            replaceImports ::
              Monad m =>
              -- | annotations for the replacement imports
              Anns ->
              -- | the replacement imports
              [LImportDecl GhcPs] ->
              -- | target module
              ParsedSource ->
              TransformT m ParsedSource
            replaceImports anns minImports t@(L l m) =
              case importAction options of
                NoImportProcessing -> return t
                _ -> do
                  -- This does all the work
                  -- retrie has a neat `insertImports' function that also
                  -- deduplicates
                  imps <- graftT anns minImports
                  -- nudge down the imports list onto a new line
                  unless (null imps) $ setEntryDPT (head imps) (DP (2, 0))
                  return $ L l m {hsmodImports = imps}

            -- | Add explict exports to the target module
            addExplicitExports ::
              Monad m =>
              -- | The list of exports to be added
              Avails ->
              -- | target module
              ParsedSource ->
              TransformT m ParsedSource
            addExplicitExports exports t@(L astLoc hsMod) =
              case exportAction options of
                NoExportProcessing -> return t
                AddExplicitExports ->
                  -- only add explicit exports if there are none
                  -- seems to work even if there is no explict module declaration
                  -- presumably because the annotations that we generate are just
                  -- unused by exactPrint
                  if isNothing currentExplicitExports then result else return t
                ReplaceExports -> result
              where
                currentExplicitExports :: Maybe (Located [LIE GhcPs])
                currentExplicitExports = hsmodExports hsMod

                -- This does all the export replacement work
                result :: Monad m => TransformT m ParsedSource
                result
                  | null exports = return t -- there is nothing exportable
                  | otherwise = do
                    -- Generate the exports list
                    exportsList <- mapM mkExportAnnT exports
                    -- add commas in between and parens around
                    mapM_ addTrailingCommaT (init exportsList)
                    lExportsList <- mkLoc exportsList >>= mkParenT unLoc

                    -- No need to do any graftTing here as we have been modifying the
                    -- annotations in the current transformation
                    return $ L astLoc hsMod {hsmodExports = Just lExportsList}

    -- | This version of the GHC function ignores implicit imports, as they
    -- cannot be parsed back in.  (There is an extraneous (implicit))
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

    -- Construct the path into which GHC's version of minimal imports is dumped
    mkMinimalImportsPath :: DynFlags -> Module -> FilePath
    mkMinimalImportsPath dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise = basefn
      where
        basefn =
          "smuggler-" ++ moduleNameString (moduleName this_mod) ++ "."
            ++ fromMaybe "smuggler" (newExtension options)
            ++ ".imports"

    options :: Options
    options = parseCommandLineOptions clopts

    strBufToStr :: StringBuffer -> String
    strBufToStr sb@(StringBuffer _ len _) = lexemeToString sb len
