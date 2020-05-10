module Smuggler.Import where

import Control.Monad ( unless )
import GHC ( HsModule(hsmodImports), GenLocated (L), GhcPs, LImportDecl, Located )
import Language.Haskell.GHC.ExactPrint.Transform
    ( logTr, graftT, getAnnsT, setEntryDPT, TransformT )
import Language.Haskell.GHC.ExactPrint.Types ( Anns, DeltaPos(DP) )
import Smuggler.Options ( ImportAction(..) )

-- | Replace a target module's imports
--   See <https://github.com/facebookincubator/retrie/blob/master/Retrie/CPP.hs>
replaceImports ::
  Monad m =>
  ImportAction ->
  -- | the annotations for the imports
  Anns ->
  -- | the replacement imports
  [LImportDecl GhcPs] ->
  -- | target module
  Located (HsModule GhcPs) ->
  TransformT m (Located (HsModule GhcPs))
replaceImports action anns minImports t@(L l m) =
  case action of
    NoImportProcessing -> return t
    _ -> do
      -- nudge down the imports list onto a new line
      unless (null minImports) $ setEntryDPT (head minImports) (DP (2, 0))
      imps <- graftT anns minImports
      return $ L l m {hsmodImports = imps}
