module Smuggler2.Anns
  ( mkLIEVarFromNameT,
    addExportDeclAnnT,
    addCommaT,
    mkLoc,
    mkParenT,
    setAnnsForT,
    swapEntryDPT
  )
where

import Data.Generics as SYB (Data)
import qualified Data.Map.Strict as Map (alter, fromList, insert, lookup, toList, union)
import Data.Maybe (fromMaybe)
import GHC (AnnKeywordId (AnnCloseP, AnnComma, AnnOpenP, AnnVal), GhcPs, IE (IEVar),
            IEWrappedName (..), Name)
import Language.Haskell.GHC.ExactPrint (Annotation (annEntryDelta, annPriorComments, annsDP),
                                        TransformT, addSimpleAnnT, modifyAnnsT, uniqueSrcSpanT)
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (..), KeywordId (G), annNone, mkAnnKey,
                                              noExt)
import OccName (HasOccName (occName), OccName (occNameFS))
import RdrName (mkVarUnqual)
import SrcLoc (GenLocated (L), Located)

{-
-- explicit version
mkLIEVarFromNameT :: Monad m => Name -> TransformT m (Located (IE GhcPs))
mkLIEVarFromNameT name = do
  -- Could use only one loc as it would be used on different constructors
  -- and not, therefore, get overwritten on subsequent uses.
  locIEVar <- uniqueSrcSpanT
  locIEName <- uniqueSrcSpanT
  locUnqual <- uniqueSrcSpanT
  return $
    L
      locIEVar
      ( IEVar
          noExt
          ( L
              locIEName
              (IEName (L locUnqual (mkVarUnqual ((occNameFS . occName) name))))
          )
      )
-}

-- Using mkLoc adds empty annotations too
mkLIEVarFromNameT :: Monad m => Name -> TransformT m (Located (IE GhcPs))
mkLIEVarFromNameT name = do
  lname <- mkLoc (mkVarUnqual ((occNameFS . occName) name))
  liename <- mkLoc (IEName lname)
  mkLoc (IEVar noExt liename)

-- TODO: This works for common cases (IEVar + IEName), but doesn't handle IEThingAbs,
-- IEThingWith, IEThingAll, IEModuleContents, IEGroup, IEDoc, IEDocNamed, XIE,
-- in all their IEName/IEPattAern and IEType variations.  But since it is only
-- creating a list of exportable things, perhaps that is OK.
addExportDeclAnnT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addExportDeclAnnT (L _ (IEVar _ (L _ (IEName x)))) =
  addSimpleAnnT x (DP (1, 2)) [(G AnnVal, DP (0, 0))]
{-
 -- Comment this out for now, so that it breaks if there is an interesting case
addExportDeclAnnT (L _ (IEVar _ (L _ (IEPattern x)))) =
  addSimpleAnnT x (DP (1, 2)) [(G AnnPattern, DP (0, 0))] -- TODO:: check that AnnPattern is correct
addExportDeclAnnT (L _ (IEVar _ (L _ (IEType x)))) =
  addSimpleAnnT x (DP (1, 2)) [(G AnnType, DP (0, 0))] -- TODO:: check that AnnType is correct
-}

-- mkParentT is used instead
addCommaT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addCommaT x = addSimpleAnnT x (DP (0, 0)) [(G AnnComma, DP (0, 0))]

{-
-- add an opening+closing parenthesis annotation.  mkParenT is used instead
addParensT :: Monad m => Located [Located (IE GhcPs)] -> TransformT m ()
addParensT x =
  addSimpleAnnT
    x
    (DP (0, 1))
    [(G AnnOpenP, DP (0, 0)), (G AnnCloseP, DP (0, 1))]
-}

-------------------------------------------------------------------------------
-- From retrie

-- | `mkLoc` generates a unique location and wraps the given ast chunk with that location
-- Also adds an empty annotation at that location
mkLoc :: (Data e, Monad m) => e -> TransformT m (Located e)
mkLoc e = do
  le <- L <$> uniqueSrcSpanT <*> pure e
  setAnnsForT le []

-- | Add an open and close paren annotation to a located thing
mkParenT ::
  (Data x, Monad m) =>
  (Located x -> x) ->
  Located x ->
  TransformT m (Located x)
mkParenT k e = do
  pe <- mkLoc (k e)
  -- There may be some other way of getting gap in front of the paren?
  _ <- setAnnsForT pe [(G GHC.AnnOpenP, DP (0, 1)), (G GHC.AnnCloseP, DP (0, 1))]
  swapEntryDPT e pe
  return pe

setAnnsForT ::
  (Data e, Monad m) =>
  Located e ->
  [(KeywordId, DeltaPos)] ->
  TransformT m (Located e)
setAnnsForT e anns = modifyAnnsT (Map.alter f (mkAnnKey e)) >> return e
  where
    f Nothing = Just annNone {annsDP = anns}
    f (Just a) =
      Just
        a
          { annsDP =
              Map.toList $
                Map.union (Map.fromList anns) (Map.fromList (annsDP a))
          }

swapEntryDPT ::
  (Data a, Data b, Monad m) => Located a -> Located b -> TransformT m ()
swapEntryDPT a b = modifyAnnsT $ \anns ->
  let akey = mkAnnKey a
      bkey = mkAnnKey b
      aann = fromMaybe annNone $ Map.lookup akey anns
      bann = fromMaybe annNone $ Map.lookup bkey anns
   in Map.insert
        akey
        aann
          { annEntryDelta = annEntryDelta bann,
            annPriorComments = annPriorComments bann
          }
        $ Map.insert
          bkey
          bann
            { annEntryDelta = annEntryDelta aann,
              annPriorComments = annPriorComments aann
            }
          anns
