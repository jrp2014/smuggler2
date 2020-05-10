module Smuggler.Export (addExplicitExports) where

import Avail ( AvailInfo, availNamesWithSelectors )
import Control.Monad ( unless )
import Data.Maybe ( isNothing )
import DynFlags ( DynFlags )
import GHC ( GenLocated(L), Located, Name, HsModule(hsmodExports) )
import Language.Haskell.GHC.ExactPrint.Transform ( runTransform )
import Language.Haskell.GHC.ExactPrint.Types ( Anns, GhcPs )
import OccName ()
import RdrName ()
import Smuggler.Options ( ExportAction(..) )
import Smuggler.Anns
    ( addCommaT, addExportDeclAnnT, addParensT, mkLIEVarFromNameT )

-- See https://www.machinesung.com/scribbles/terser-import-declarations.html
-- and https://www.machinesung.com/scribbles/ghc-api.html


addExplicitExports
  :: DynFlags
  -> ExportAction
  -> [AvailInfo]
  -> (Anns, Located (HsModule GhcPs))
  -> (Anns, Located (HsModule GhcPs))
addExplicitExports dflags action exports p@(anns, L astLoc hsMod) =
  case action of
    NoExportProcessing -> p
    AddExplicitExports ->
      if isNothing currentExplicitExports then (anns', ast') else p
    ReplaceExports -> (anns', ast')
 where
  currentExplicitExports  = hsmodExports hsMod

  (ast', (anns', _n), _s) = runTransform anns $ do

    let names = mkNamesFromAvailInfos exports

    exportsList <- mapM mkLIEVarFromNameT names
    mapM_ addExportDeclAnnT exportsList
    unless (null exportsList) $ mapM_ addCommaT (init exportsList)

    let lExportsList = L astLoc exportsList
        hsMod'       = hsMod { hsmodExports = Just lExportsList }
    unless (null exportsList) $ addParensT lExportsList

    return (L astLoc hsMod')


mkNamesFromAvailInfos :: [AvailInfo] -> [Name]
mkNamesFromAvailInfos = concatMap availNamesWithSelectors
--Produces all names from the availability information (including overloaded selectors)
--To exclude overloaded selector use availNames
