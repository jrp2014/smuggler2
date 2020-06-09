-- |
-- Description: Utility functions for transforming and manipulating
--              'ghc-exactprint' 'Language.Haskell.GHC.ExactPrint.Anns'
module Smuggler2.Anns
  ( mkLocWithAnns,
    mkLoc,
    mkParenT,
    setAnnsForT,
    swapEntryDPT,
  )
where

import Data.Generics as SYB (Data)
import qualified Data.Map.Strict as Map (alter, fromList, insert, lookup, toList, union)
import Data.Maybe (fromMaybe)
import GHC (AnnKeywordId (AnnCloseP, AnnOpenP))
import GhcPlugins (GenLocated (L), Located)
import Language.Haskell.GHC.ExactPrint
  ( Annotation (annEntryDelta, annPriorComments, annsDP),
    TransformT,
    modifyAnnsT,
    uniqueSrcSpanT,
  )
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (..), KeywordId (G), annNone, mkAnnKey)

-- Inspired by retrie

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

-- | Set the `ghc-exactprint` annotations for a 'Located' thing
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

-- | Swap two 'Located' things' relative position tage ('DeltaPos')
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
