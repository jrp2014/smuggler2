{-# LANGUAGE LambdaCase #-}

module Smuggler.Plugin
  ( plugin,
  )
where

import Avail (Avails, availNamesWithSelectors)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (isNothing)
import DynFlags (DynFlags (dumpDir), HasDynFlags (getDynFlags))
import GHC (GenLocated (L), GhcPs, HsModule (hsmodExports, hsmodImports),
            ImportDecl (ideclHiding, ideclImplicit), LIE, LImportDecl, Located,
            ModSummary (ms_hspp_buf, ms_hspp_file, ms_mod), Module (moduleName), Name,
            moduleNameString, unLoc)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import IOEnv (readMutVar)
import Language.Haskell.GHC.ExactPrint (Anns, TransformT, exactPrint, graftT, runTransform,
                                        setEntryDPT)
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (DP))
import Outputable (Outputable (ppr), neverQualify, printForUser, vcat)
import Plugins (CommandLineOption, Plugin (pluginRecompile, typeCheckResultAction), defaultPlugin,
                purePlugin)
import RnNames (ImportDeclUsage, findImportUsage, getMinimalImports)
import Smuggler.Anns (addCommaT, addExportDeclAnnT, mkLIEVarFromNameT, mkLoc, mkParenT)
import Smuggler.Options (ExportAction (AddExplicitExports, NoExportProcessing, ReplaceExports),
                         ImportAction (MinimiseImports, NoImportProcessing),
                         Options (exportAction, importAction, newExtension),
                         parseCommandLineOptions)
import Smuggler.Parser (runParser)
import StringBuffer (StringBuffer (StringBuffer), lexemeToString)
import System.FilePath ((-<.>), (</>))
import System.IO (IOMode (WriteMode), withFile)
import TcRnTypes (RnM, TcGblEnv (tcg_exports, tcg_rn_imports, tcg_used_gres), TcM)


plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = smugglerPlugin,
      pluginRecompile = purePlugin -- ^ Don't force recompilation.  [Is this the right approach?]
    }


-- | The plugin itself
smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clopts modSummary tcEnv = do
  -- bail quickly, if nothing to do
  when ((importAction options == NoImportProcessing) && (exportAction options == NoExportProcessing)) (pure ())

  -- Get the imports and their usage
  let imports = tcg_rn_imports tcEnv
  uses <- readMutVar $ tcg_used_gres tcEnv
  let usage = findImportUsage imports uses

  -- Dump GHC's view of a minimal set of imports
  dflags <- getDynFlags
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

      --modFileContents <- readFile modulePath

      -- Get the pre-processed source code
      let modFileContents = case ms_hspp_buf modSummary of
            -- Not clear under what circumstances this could happen
            Nothing       -> error "smuggler: missing source file: " ++ modulePath
            Just contents -> strBufToStr contents

      -- parse the whole module
      runParser dflags modulePath modFileContents >>= \case
        Left () -> pure () -- do nothing if file is invalid Haskell
        Right (annsHsMod, astHsMod) -> do

          -- read the dumped file of minimal imports
          minImpFileContents <- readFile minImpFilePath

          -- parse the minimal imports file
          runParser dflags minImpFilePath minImpFileContents >>= \case
            Left () -> pure ()
            Right (annsImpMod, L _ impMod) -> do

              -- What is exported
              -- TODO: persumably this is not the same as what is exportable,
              -- which means that the rewrite exports action will not do much
              let exports = tcg_exports tcEnv

              let (astHsMod', (annsHsMod', _locIndex), _log) = runTransform annsHsMod $ do
                    replaceImports annsImpMod (hsmodImports impMod) astHsMod
                      >>= addExplicitExports exports

              let newContent = exactPrint astHsMod' annsHsMod'
              case newExtension options of
                Nothing  -> writeFile modulePath newContent
                Just ext -> writeFile (modulePath -<.> ext) newContent

    --




    -- | Replace a target module's imports
    --   See <https://github.com/facebookincubator/retrie/blob/master/Retrie/CPP.hs>
    replaceImports ::
      Monad m =>
      -- | the annotations for the imports
      Anns ->
      -- | the replacement imports
      [LImportDecl GhcPs] ->
      -- | target module
      Located (HsModule GhcPs) ->
      TransformT m (Located (HsModule GhcPs))
    replaceImports anns minImports t@(L l m) =
      case action of
        NoImportProcessing -> return t
        _ -> do
          imps <- graftT anns minImports
          -- nudge down the imports list onto a new line
          unless (null imps) $ setEntryDPT (head imps) (DP (2, 0))
          return $ L l m {hsmodImports = imps}
       where
         action = importAction options



    -- Add explict exports to the target module
    addExplicitExports ::
      Monad m =>
      -- | The list of exports to be added
      Avails ->
      -- | target module
      Located (HsModule GhcPs) ->
      TransformT m (Located (HsModule GhcPs))
    addExplicitExports exports t@(L astLoc hsMod) =
      case action of
        NoExportProcessing -> return t
        AddExplicitExports -> -- only add explicit exports if there are none
          if isNothing currentExplicitExports then result else return t
        ReplaceExports -> result
      where

        action = exportAction options

        currentExplicitExports :: Maybe (Located [LIE GhcPs])
        currentExplicitExports = hsmodExports hsMod

        names :: [Name]
        names = reverse $ mkNamesFromAvailInfos exports -- TODO check the ordering

        -- Produces all names from the availability information (including overloaded selectors)
        -- To exclude overloaded selector use availNames
        mkNamesFromAvailInfos :: Avails -> [Name]
        mkNamesFromAvailInfos = concatMap availNamesWithSelectors

        -- This does all the work
        result :: Monad m => TransformT m (Located (HsModule GhcPs))
        result
          | null names = return t
          | otherwise = do

            -- Generate the (annotated) exports list
            exportsList <- mapM mkLIEVarFromNameT names
            mapM_ addExportDeclAnnT exportsList
            mapM_ addCommaT (init exportsList)

            lExportsList <- mkLoc exportsList >>= mkParenT unLoc

            -- Graft back
            -- Doesn't seeem necessary, at least if the exports don't already exist
            --anns <- getAnnsT
            --lExportsList' <- graftT anns lExportsList
            --return $ L astLoc hsMod {hsmodExports = Just lExportsList'}

            return $ L astLoc hsMod {hsmodExports = Just lExportsList}



    -- This version of the GHC function ignores implicit imports, as the result
    -- cannot be parsed back in.  (There is an extraneous (implicit)')
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
          _                    -> True

        keepInstanceOnlyImports :: Bool
        keepInstanceOnlyImports = importAction options /= MinimiseImports

        letThrough :: ImportDecl pass -> Bool
        letThrough i = notImplicit i && (keepInstanceOnlyImports || notInstancesOnly i)

    -- | Make a path for the file into which minmal imports are dumped
    -- It has a "smuggler-" prefix to avoid rewritng the filees that GHC
    -- generates with the -fdump-minimal-imports flag
    mkFilePath :: DynFlags -> Module -> FilePath
    mkFilePath dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise = basefn
      where
        basefn = "smuggler-" ++ moduleNameString (moduleName this_mod) ++ ".imports"


    -- | Command line option translation
    options :: Options
    options = parseCommandLineOptions clopts

    -- | Decode StringBuffer as UTF-8 into a String
    strBufToStr :: StringBuffer -> String
    strBufToStr sb@(StringBuffer _ len _) = lexemeToString sb len
