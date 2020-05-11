module Smuggler.Export (addExplicitExports) where

import Avail (AvailInfo, availNamesWithSelectors)
import Control.Monad (guard)
import Data.Maybe (isNothing)
import GHC (GenLocated (L), HsModule (hsmodExports), LIE, Located, Name, unLoc)
import Language.Haskell.GHC.ExactPrint.Transform (TransformT, graftT, runTransform, getAnnsT)
import Language.Haskell.GHC.ExactPrint.Types (Anns, GhcPs)
import Smuggler.Anns
  ( addCommaT,
    addExportDeclAnnT,
    addParensT,
    mkLIEVarFromNameT,
    mkLoc,
    mkParen,
  )
import Smuggler.Options (ExportAction (..))

-- See https://www.machinesung.com/scribbles/terser-import-declarations.html
-- and https://www.machinesung.com/scribbles/ghc-api.html

addExplicitExports ::
  Monad m =>
  ExportAction ->
  -- | The exports to be added
  [AvailInfo] ->
  -- | target module
  Located (HsModule GhcPs) ->
  TransformT m (Located (HsModule GhcPs))
addExplicitExports action exports t@(L astLoc hsMod) =
  case action of
    NoExportProcessing -> return t
    AddExplicitExports -> -- only add explicit exports if there are none
      if isNothing currentExplicitExports then result else return t
    ReplaceExports -> result
  where

    currentExplicitExports :: Maybe (Located [LIE GhcPs])
    currentExplicitExports = hsmodExports hsMod

    names :: [Name]
    names = reverse $ mkNamesFromAvailInfos exports -- TODO check the ordering

    result :: Monad m => TransformT m (Located (HsModule GhcPs))
    result
      | null names = return t
      | otherwise = do

        -- Generate the (annotated) exports list
        exportsList <- mapM mkLIEVarFromNameT names
        mapM_ addExportDeclAnnT exportsList
        mapM_ addCommaT (init exportsList)

        lExportsList <- mkLoc exportsList >>= mkParen unLoc

        -- Graft back
        -- Doesn't seeem necessary, at least if the exports don't already exist
        --anns <- getAnnsT
        --lExportsList' <- graftT anns lExportsList
        --return $ L astLoc hsMod {hsmodExports = Just lExportsList'}

        return $ L astLoc hsMod {hsmodExports = Just lExportsList}

-- | Produces all names from the availability information (including overloaded selectors)
--   To exclude overloaded selector use availNames
mkNamesFromAvailInfos :: [AvailInfo] -> [Name]
mkNamesFromAvailInfos = concatMap availNamesWithSelectors
