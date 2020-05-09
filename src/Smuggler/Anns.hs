{-# LANGUAGE RecordWildCards #-}

module Smuggler.Anns
  ( removeAnnAtLoc
  , removeTrailingCommas
  , removeLocatedKeywordT
  , setAnnsFor
  , mkLoc
  , mkParen
  , swapEntryDPT
  , transferEntryAnnsT
  , transferEntryDPT
  , transferEntryDP
  , addAllAnnsT
  , addAllAnns
  , isComma
  , isCommentKeyword
  , isCommentAnnotation
  , hasComments
  , transferAnnsT
  , setEntryDPT
  , setEntryDP
  )
where

import Data.Function (on)
import Data.Generics as SYB (Data)
import Data.List (find, groupBy)
import qualified Data.Map.Strict as Map (empty, alter, filterWithKey, findWithDefault, fromList, insert,
                                         lookup, toList, union)
import Data.Maybe (fromMaybe)
import qualified GHC (AnnKeywordId (..))
import Language.Haskell.GHC.ExactPrint (AnnKey (..), Annotation (..), Anns, TransformT, modifyAnnsT,
                                        uniqueSrcSpanT, getAnnsT)
import Language.Haskell.GHC.ExactPrint.Types (AnnConName (..), DeltaPos (..), KeywordId (..),
                                              annNone, mkAnnKey)
import Language.Haskell.GHC.ExactPrint.Transform (runTransform, graftT)
import Language.Haskell.GHC.ExactPrint.Utils (annLeadingCommentEntryDelta)
import SrcLoc (GenLocated (..), Located, SrcSpan (RealSrcSpan), srcSpanEndLine, srcSpanStartCol,
               srcSpanStartLine)


-------------------------------------------------------------------------------

setAnnsFor
  :: (Data e, Monad m)
  => Located e
  -> [(KeywordId, DeltaPos)]
  -> TransformT m (Located e)
setAnnsFor e anns = modifyAnnsT (Map.alter f (mkAnnKey e)) >> return e
 where
  f Nothing  = Just annNone { annsDP = anns }
  f (Just a) = Just a
    { annsDP = Map.toList
                 $ Map.union (Map.fromList anns) (Map.fromList (annsDP a))
    }

mkLoc :: (Data e, Monad m) => e -> TransformT m (Located e)
mkLoc e = do
  le <- L <$> uniqueSrcSpanT <*> pure e
  setAnnsFor le []

mkParen
  :: (Data x, Monad m)
  => (Located x -> x)
  -> Located x
  -> TransformT m (Located x)
mkParen k e = do
  pe <- mkLoc (k e)
  _ <- setAnnsFor pe [(G GHC.AnnOpenP, DP (0, 0)), (G GHC.AnnCloseP, DP (0, 0))]
  swapEntryDPT e pe
  return pe


swapEntryDPT
  :: (Data a, Data b, Monad m) => Located a -> Located b -> TransformT m ()
swapEntryDPT a b = modifyAnnsT $ \anns ->
  let akey = mkAnnKey a
      bkey = mkAnnKey b
      aann = fromMaybe annNone $ Map.lookup akey anns
      bann = fromMaybe annNone $ Map.lookup bkey anns
  in  Map.insert
          akey
          aann { annEntryDelta    = annEntryDelta bann
               , annPriorComments = annPriorComments bann
               }
        $ Map.insert
            bkey
            bann { annEntryDelta    = annEntryDelta aann
                 , annPriorComments = annPriorComments aann
                 }
            anns

-- The following definitions are all the same as the ones from ghc-exactprint,
-- but the types are liberalized from 'Transform a' to 'TransformT m a'.
transferEntryAnnsT
  :: (Data a, Data b, Monad m)
  => (KeywordId -> Bool)        -- transfer Anns matching predicate
  -> Located a                  -- from
  -> Located b                  -- to
  -> TransformT m ()
transferEntryAnnsT p a b = do
  transferEntryDPT a b
  transferAnnsT p a b

-- | 'Transform' monad version of 'transferEntryDP'
transferEntryDPT
  :: (Data a, Data b, Monad m) => Located a -> Located b -> TransformT m ()
transferEntryDPT a b = modifyAnnsT (transferEntryDP a b)

-- This function fails if b is not in Anns, which seems dumb, since we are inserting it.
transferEntryDP :: (Data a, Data b) => Located a -> Located b -> Anns -> Anns
transferEntryDP a b anns = setEntryDP b dp anns'
 where
  maybeAnns = do -- Maybe monad
    anA <- Map.lookup (mkAnnKey a) anns
    let anB  = Map.findWithDefault annNone (mkAnnKey b) anns
        anB' = anB { annEntryDelta = DP (0, 0) }
    return (Map.insert (mkAnnKey b) anB' anns, annLeadingCommentEntryDelta anA)
  (anns', dp) = fromMaybe
    (error $ "transferEntryDP: lookup failed: " ++ show (mkAnnKey a))
    maybeAnns

addAllAnnsT
  :: (Data a, Data b, Monad m) => Located a -> Located b -> TransformT m ()
addAllAnnsT a b = modifyAnnsT (addAllAnns a b)

addAllAnns :: (Data a, Data b) => Located a -> Located b -> Anns -> Anns
addAllAnns a b anns =
  fromMaybe
      (  error
      $  "addAllAnns: lookup failed: "
      ++ show (mkAnnKey a)
      ++ " or "
      ++ show (mkAnnKey b)
      )
    $ do
        ann <- Map.lookup (mkAnnKey a) anns
        case Map.lookup (mkAnnKey b) anns of
          Just ann' ->
            return $ Map.insert (mkAnnKey b) (ann `annAdd` ann') anns
          Nothing -> return $ Map.insert (mkAnnKey b) ann anns
 where
  annAdd ann ann' = ann'
    { annEntryDelta        = annEntryDelta ann
    , annPriorComments     = ((++) `on` annPriorComments) ann ann'
    , annFollowingComments = ((++) `on` annFollowingComments) ann ann'
    , annsDP               = ((++) `on` annsDP) ann ann'
    }

trimT :: Data ast => Anns -> ast -> (Anns, ast)
trimT anns ast = (anns', ast')
  where
    (ast', (anns', _), _) = runTransform Map.empty $ graftT anns ast


      
isComma :: KeywordId -> Bool
isComma (G GHC.AnnComma) = True
isComma _                = False

isCommentKeyword :: KeywordId -> Bool
isCommentKeyword (AnnComment _) = True
isCommentKeyword _              = False

isCommentAnnotation :: Annotation -> Bool
isCommentAnnotation Ann {..} =
  (not . null $ annPriorComments)
    || (not . null $ annFollowingComments)
    || any (isCommentKeyword . fst) annsDP

hasComments :: (Data a, Monad m) => Located a -> TransformT m Bool
hasComments e = do
  anns <- getAnnsT
  let b = isCommentAnnotation <$> Map.lookup (mkAnnKey e) anns
  return $ fromMaybe False b

transferAnnsT
  :: (Data a, Data b, Monad m)
  => (KeywordId -> Bool)        -- transfer Anns matching predicate
  -> Located a                  -- from
  -> Located b                  -- to
  -> TransformT m ()
transferAnnsT p a b = modifyAnnsT f
 where
  bKey = mkAnnKey b
  f anns = fromMaybe anns $ do
    anA <- Map.lookup (mkAnnKey a) anns
    anB <- Map.lookup bKey anns
    let anB' = anB { annsDP = annsDP anB ++ filter (p . fst) (annsDP anA) }
    return $ Map.insert bKey anB' anns

-- | 'Transform' monad version of 'getEntryDP'
setEntryDPT :: (Data a, Monad m) => Located a -> DeltaPos -> TransformT m ()
setEntryDPT ast dp = do
  modifyAnnsT (setEntryDP ast dp)

-- | The setEntryDP that comes with exactprint does some really confusing
-- entry math around comments that I'm not convinced is either correct or useful.
setEntryDP :: Data a => Located a -> DeltaPos -> Anns -> Anns
setEntryDP x dp anns = Map.alter (Just . f . fromMaybe annNone) k anns
 where
  k = mkAnnKey x
  f ann = case annPriorComments ann of
    []          -> ann { annEntryDelta = dp }
    (c, _) : cs -> ann { annPriorComments = (c, dp) : cs }


-------------------------------------------------------------------------------







removeAnnAtLoc :: Int -> Int -> Anns -> Anns
removeAnnAtLoc line col = Map.filterWithKey (\k _ -> matchKey k)
 where
  matchKey :: AnnKey -> Bool
  matchKey (AnnKey (RealSrcSpan rss) _) =
    not (srcSpanStartLine rss == line && srcSpanStartCol rss == col)
  matchKey _ = True

removeLocatedKeywordT
  :: (Data a, Monad m) => KeywordId -> Located a -> TransformT m ()
removeLocatedKeywordT kw ast = modifyAnnsT (removeLocatedKeyword kw ast)

removeLocatedKeyword :: (SYB.Data a) => KeywordId -> Located a -> Anns -> Anns
removeLocatedKeyword kw ast anns = case Map.lookup (mkAnnKey ast) anns of
  Nothing -> anns
  Just an -> case find isKeyword (annsDP an) of
    Nothing -> anns
    Just _  -> Map.insert
      (mkAnnKey ast)
      (an { annsDP = filter (not . isKeyword) (annsDP an) })
      anns
   where
    isKeyword (kw', _) | kw' == kw = True
    isKeyword _        = False

-- Not needed; using the ghc-exactprint library version
removeTrailingCommas :: Anns -> Anns
removeTrailingCommas =
  Map.fromList
    . concatMap removeIfImportDecl
    . groupBy withinSrcSpan
    . Map.toList
 where
  removeIfImportDecl :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
  removeIfImportDecl gAnns | any isImportDecl gAnns = removeTrailingComma gAnns
                           | otherwise              = gAnns

  removeTrailingComma :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
  removeTrailingComma [] = []
  -- The import anns are held in the shape of "(:) ... (IEName, IEVar, Unqual)"
  -- we want to pattern match on the last entry and remove the list separator ','
  -- if it is present
  removeTrailingComma [x, (annKey, ann), z] =
    [ x
    , (annKey, ann { annsDP = filter (not . isTrailingComma) (annsDP ann) })
    , z
    ]
  removeTrailingComma (x : xs) = x : removeTrailingComma xs

  isImportDecl :: (AnnKey, Annotation) -> Bool
  isImportDecl (AnnKey _ (CN "ImportDecl"), _) = True
  isImportDecl _                               = False

  isTrailingComma :: (KeywordId, DeltaPos) -> Bool
  isTrailingComma (G GHC.AnnComma, _) = True
  isTrailingComma _                   = False

  withinSrcSpan :: (AnnKey, Annotation) -> (AnnKey, Annotation) -> Bool
  withinSrcSpan (AnnKey (RealSrcSpan x) _, _) (AnnKey (RealSrcSpan y) _, _) =
    srcSpanStartLine x
      == srcSpanStartLine y
      || srcSpanEndLine x
      == srcSpanEndLine y
  withinSrcSpan _ _ = True
