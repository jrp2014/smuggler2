-- |
-- Description: Utility functions for generating an export list ast and
-- associated Anns.  It's a bit fiddlier than it could be because ghc's
-- functions for producing exportable things generates @AvailInfo@ from which we
-- need to reconstitue @IEWrappedName@ and then @IE@
module Smuggler2.Exports
  ( mkExportAnnT
  )
where

import Avail ( AvailInfo(..) )
import GHC
    ( AnnKeywordId(AnnCloseP, AnnVal, AnnType, AnnPattern, AnnDotdot,
                   AnnOpenP),
      GhcPs,
      IE(IEThingAbs, IEVar, IEThingAll),
      IEWrappedName(IEName, IEType, IEPattern),
      LIEWrappedName,
      RdrName )
import GhcPlugins ( Located, mkVarUnqual )
import Language.Haskell.GHC.ExactPrint ( TransformT )
import Language.Haskell.GHC.ExactPrint.Types
    ( noExt, DeltaPos(DP), KeywordId(G) )
import Lexeme ( isLexSym )
import Name
    ( Name,
      OccName(occNameFS),
      getOccString,
      isDataOcc,
      isSymOcc,
      isTcOcc,
      HasOccName(occName) )
import Smuggler2.Anns ( mkLocWithAnns, mkLoc )

-- | Generates the annotations for a name, wrapping () around symbollic names
mkLIEName ::
  Monad m =>
  Name ->
  TransformT m (LIEWrappedName RdrName)
mkLIEName name
  | isTcOcc occ && isSymOcc occ = do
    lname <-
      mkLocWithAnns
        (mkVarUnqual nameFS)
        (DP (0, 0)) -- for a gap afer @type@
        ann
    mkLocWithAnns (IEType lname) (DP (1, 2)) [(G AnnType, DP (0, 0))]
  | isDataOcc occ = do
    lname <-
      mkLocWithAnns
        (mkVarUnqual nameFS)
        (DP (0, 1)) -- for a gap after @pattern@
        ann
    mkLocWithAnns (IEPattern lname) (DP (1, 2)) [(G AnnPattern, DP (0, 0))]
  | otherwise = do
    lname <-
      mkLocWithAnns
        (mkVarUnqual nameFS)
        (DP (0, 0))
        ann
    mkLocWithAnns (IEName lname) (DP (1, 2)) []
  where
    occ = occName name
    nameFS = occNameFS occ
    ann =
      if isLexSym nameFS -- infix type or data constructor / identifier, so add ()
        then [(G AnnOpenP, DP (0, 0)), (G AnnVal, DP (0, 0)), (G AnnCloseP, DP (0, 0))]
        else [(G AnnVal, DP (0, 0))]

-- | Uses an exportable thing to generate the corresponding
-- piece of (annotated) AST.
mkExportAnnT :: (Monad m) => AvailInfo -> TransformT m (Located (IE GhcPs))
-- Ordinary identifier
mkExportAnnT (Avail name) = do
  liename <- mkLIEName name
  mkLoc (IEVar noExt liename)

-- A type or class.  Since we expect @name@ to be in scope, it should be the head
-- of @names@
mkExportAnnT (AvailTC name names fieldlabels) = do
  liename <- mkLIEName name

  -- Could export pieces explicitly, but this becomes ugly;
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
    ([_typeclass], []) -> mkLoc (IEThingAbs noExt liename)
    -- A type class with no pieces, but with field selectors.  A record type?
    ([_typeclass], _fl) -> lienameWithWildcard
    -- A type class with pieces
    (typeorclass : _pieces, _fl) ->
      if name == typeorclass -- check AvailTC invariant
        then lienameWithWildcard
        else
          error $
            "smuggler: broken AvailTC invariant: "
              ++ getOccString name
              ++ "/="
              ++ getOccString typeorclass
