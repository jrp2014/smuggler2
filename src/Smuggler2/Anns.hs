{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Smuggler2.Anns
  ( mkExportAnnT,
    mkLIEVarFromNameT,
    addExportDeclAnnT,
    addCommaT,
    mkLoc,
    mkParenT,
    setAnnsForT,
    swapEntryDPT,
  )
where

import Avail
import Data.Generics as SYB (Data)
import qualified Data.Map.Strict as Map (alter, fromList, insert, lookup, toList, union)
import Data.Maybe (fromMaybe)
import GHC
  ( AnnKeywordId (AnnCloseP, AnnComma, AnnDotdot, AnnOpenP, AnnVal),
    GhcPs,
    IE (..),
    IEWrappedName (..),
    Name,
  )
import Language.Haskell.GHC.ExactPrint
  ( Annotation (annEntryDelta, annPriorComments, annsDP),
    TransformT,
    addSimpleAnnT,
    modifyAnnsT,
    uniqueSrcSpanT,
  )
import Language.Haskell.GHC.ExactPrint.Types
  ( DeltaPos (..),
    KeywordId (G),
    annNone,
    mkAnnKey,
    noExt,
  )
import Name (getOccString)
import OccName (HasOccName (occName), OccName (occNameFS))
import RdrName (mkVarUnqual)
import SrcLoc (GenLocated (L), Located)

-- | Uses 'AvailInfo' about an exportable thing to generate the corresponding
-- piece of (annotated) AST
mkExportAnnT :: Monad m => AvailInfo -> TransformT m (Located (IE GhcPs))
-- Ordinary identifier
mkExportAnnT (Avail name) = do
  lname <-
    mkLocWithAnns
      (mkVarUnqual ((occNameFS . occName) name))
      (DP (1, 2))
      [(G AnnVal, DP (0, 0))]
  liename <- mkLoc (IEName lname)
  mkLoc (IEVar noExt liename)

-- A type or class
mkExportAnnT (AvailTC name names fieldlabels) = do
  lname <-
    mkLocWithAnns
      (mkVarUnqual ((occNameFS . occName) name))
      (DP (1, 2))
      [(G AnnVal, DP (0, 0))]
  liename <- mkLoc (IEName lname)

  -- Could export pieces explicitly, but this becomes complicated;
  -- operators need to be wrapped in (), etc, so just export things
  -- with pieces by wildcard
  let lienameWithWildcard =
        mkLocWithAnns
          (IEThingAll noExt liename)
          (DP (0, 0))
          [(G AnnOpenP, DP (0, 0)), (G AnnDotdot, DP (0, 0)), (G AnnCloseP, DP (0, 0))]

  case (names, fieldlabels) of
    -- This case implies that the type or class is not to be in scope
    -- which should not happen as we should only be processing exportable things
    -- Alternativey, could just: mkLoc (IEThingAbs noExt liename)
    ([], _) ->
      error $
        "smuggler: trying to export type class that is not to be in scope "
          ++ getOccString name

    -- A type class with no pieces
    ([typeclass], []) -> mkLoc (IEThingAbs noExt liename)

    -- A type class with no pieces, but with field selectors.  A record type?
    ([typeclass], _fl) -> lienameWithWildcard

    -- A type class with pieces
    (typeorclass : _pieces, _fl) ->
      if name == typeorclass -- check AvailTC invariant
        then
          lienameWithWildcard
        else
          error $
            "smuggler: broken AvailTC invariant: "
              ++ getOccString name
              ++ "/="
              ++ getOccString typeorclass

-- Using mkLoc adds empty annotations too
-- TODO:: Makes everything int an IEVar; do other cases
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

-- | Generates a unique location and wraps the given ast chunk with that location
-- Also adds a DP and an annotation at that location
mkLocWithAnns :: (Data e, Monad m) => e -> DeltaPos -> [(KeywordId, DeltaPos)] -> TransformT m (Located e)
mkLocWithAnns e dp anns = do
  le <- L <$> uniqueSrcSpanT <*> pure e
  setAnnsForT le dp anns

-- | `mkLoc` generates a unique location and wraps the given ast chunk with that location
-- Also adds an empty annotation at that location
mkLoc :: (Data e, Monad m) => e -> TransformT m (Located e)
mkLoc e = mkLocWithAnns e (DP (0, 0)) []

--

-- | `mkLoc` generates a unique location and wraps the given ast chunk with that location
-- Also adds an empty annotation at that location
mkLocDP :: (Data e, Monad m) => e -> DeltaPos -> TransformT m (Located e)
mkLocDP e dp = mkLocWithAnns e dp []

-- | Add an open and close paren annotation to a located thing
mkParenT ::
  (Data x, Monad m) =>
  (Located x -> x) ->
  Located x ->
  TransformT m (Located x)
mkParenT k e = do
  pe <- mkLoc (k e)
  _ <- setAnnsForT pe (DP (0, 0)) [(G GHC.AnnOpenP, DP (0, 1)), (G GHC.AnnCloseP, DP (0, 1))]
  swapEntryDPT e pe
  return pe

setAnnsForT ::
  (Data e, Monad m) =>
  Located e ->
  DeltaPos ->
  [(KeywordId, DeltaPos)] ->
  TransformT m (Located e)
setAnnsForT e dp anns = modifyAnnsT (Map.alter f (mkAnnKey e)) >> return e
  where
    f Nothing = Just annNone {annEntryDelta = dp, annsDP = anns}
    f (Just a) =
      Just
        a
          { annEntryDelta = dp,
            annsDP =
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
