module Smuggler.Import where

import           Control.Monad                  ( unless )
import           DynFlags                       ( DynFlags )
import           GHC                            ( ieName
                                                , GhcPs
                                                , HsModule(hsmodImports)
                                                , ModuleName
                                                , hsmodName
                                                , ImportDecl(..)
                                                , LIE
                                                , LImportDecl
                                                , GhcRn )
import           Language.Haskell.GHC.ExactPrint.Transform
                                                ( removeTrailingCommaT
                                                , runTransform
                                                , TransformT
                                                , setEntryDPT
                                                , uniqueSrcSpanT
                                                , graftT
                                                )
import           Language.Haskell.GHC.ExactPrint.Types
                                                ( Anns
                                                , DeltaPos(DP)
                                                )
import           RdrName                        ( GlobalRdrElt(..) )
import           RnNames                        ( findImportUsage
                                                , ImportDeclUsage
                                                )
import           Smuggler.Anns                ( mkLIEVarFromNameT
                                                , addCommaT
                                                , addParensT
                                                )
import           Smuggler.Options               ( ImportAction(..) )
import           SrcLoc                         ( unLoc
                                                , GenLocated(L)
                                                , Located
                                                , SrcSpan
                                                  ( RealSrcSpan
                                                  , UnhelpfulSpan
                                                  )
                                                )

import Data.Function (on)
import Data.Functor.Identity
import Data.List (nubBy)




--https://github.com/facebookincubator/retrie/blob/master/Retrie/CPP.hs
{-

type AnnotatedImports = (Anns, LImportDecl GhcPs)

insertImports
  :: Monad m
  => Anns
  -> [LImportDecl GhcPs]   -- ^ imports and their annotations
  -> Located (HsModule GhcPs)    -- ^ target module
  -> TransformT m (Located (HsModule GhcPs))
insertImports anns is (L l m) = do
  imps <- graftT anns is
  let
    deduped = nubBy (eqImportDecl `on` unLoc) $ hsmodImports m ++ imps
  return $ L l m { hsmodImports = deduped }

--filterAndFlatten :: Maybe ModuleName -> [AnnotatedImports] -> AnnotatedImports
filterAndFlatten mbName anns is =
  runTransform anns  $ return . externalImps mbName
  where
    externalImps :: Maybe ModuleName -> [LImportDecl GhcPs] -> [LImportDecl GhcPs]
    externalImps (Just mn) = filter ((/= mn) . unLoc . ideclName . unLoc)
    externalImps _ = id

-}

eqImportDecl :: ImportDecl GhcPs -> ImportDecl GhcPs -> Bool
eqImportDecl x y =
  ((==) `on` unLoc . ideclName) x y
  && ((==) `on` ideclQualified) x y
  && ((==) `on` ideclAs) x y
  && ((==) `on` ideclHiding) x y
  && ((==) `on` ideclPkgQual) x y
  && ((==) `on` ideclSource) x y
  && ((==) `on` ideclSafe) x y
  -- intentionally leave out ideclImplicit and ideclSourceSrc
  -- former doesn't matter for this check, latter is prone to whitespace issues














minimiseImports
  :: DynFlags
  -> ImportAction
  -> [LImportDecl GhcRn]
  -> [GlobalRdrElt]
  -> (Anns, Located (HsModule GhcPs))
  -> (Anns, Located (HsModule GhcPs))
minimiseImports dflags action user_imports uses p@(anns, L astLoc hsMod) =
  case action of
    NoImportProcessing -> p
    _                  -> (anns', L astLoc hsMod')
--      trace ("usage\n" ++ showSDoc dflags (ppr usage)) (anns', L astLoc hsMod')

 where

  imports :: [LImportDecl GhcPs]
  imports = hsmodImports hsMod

  -- ImportDeclUsage = (LImportDecl GhcRn, used: [AvailInfo], unused: [Name])
  usage :: [ImportDeclUsage]
  usage             = findImportUsage user_imports uses

  (anns', imports') = findUsedImports anns imports usage
  hsMod'            = hsMod { hsmodImports = imports' }

  findUsedImports
    :: Anns
    -> [LImportDecl GhcPs]
    -> [ImportDeclUsage]
    -> (Anns, [LImportDecl GhcPs])
  findUsedImports anns [] [] = (anns, [])
  findUsedImports anns (p : ps) (r : rs) =
    (anns'', usedImports ++ usedImports')
   where
    (anns' , usedImports ) = usedImport dflags action anns p r
    (anns'', usedImports') = findUsedImports anns' ps rs

-- TODO: rewrite this as a transform, like Export?

-- TODO: reuse more logic from GHC. Is it possible?
usedImport
  :: DynFlags
  -> ImportAction
  -> Anns
  -> LImportDecl GhcPs
  -> ImportDeclUsage
  -> (Anns, [LImportDecl GhcPs])
usedImport _ _ anns impPs (L (UnhelpfulSpan _) _, _, _) = (anns, [impPs])
usedImport dynflags action anns impPs@(L (RealSrcSpan locPs) declPs) (L (RealSrcSpan _) declRn, used, unused)

-- unused applies to an explicit import list.  It will be null for a simple module
-- import

  | -- Do not remove `import M ()`
    Just (False, L _ []) <- ideclHiding declRn
  = (anns, [impPs])
  | -- Note [Do not warn about Prelude hiding]
    -- TODO: add ability to support custom prelude
    Just (True, L _ hides) <- ideclHiding declRn
  , not (null hides)
  , ideclImplicit declRn -- pRELUDE_NAME == unLoc (ideclName decl)
  = (anns, [impPs])
  | -- Nothing used
    null used
  = case action of
    PreserveInstanceImports -> case ideclHiding declRn of
      Nothing -> -- add (), to import instances only
        let (ast', (anns', _n), _s) = runTransform anns $ do
              lIEloc <- uniqueSrcSpanT
              let lIEs = L lIEloc [] :: Located [LIE GhcPs]
              addParensT lIEs
              let declPs' = declPs { ideclHiding = Just (False, lIEs) }
              let impPs'  = L (RealSrcSpan locPs) declPs'
              return [impPs']
        in  (anns', ast')
      Just (False, L lIEloc _) -> -- just leave the ()
        let (ast', (anns', _n), _s) = runTransform anns $ do
              let noLIEs = L lIEloc [] :: Located [LIE GhcPs]
              addParensT noLIEs
              let declPs' = declPs { ideclHiding = Just (False, noLIEs) }
              let impPs'  = L (RealSrcSpan locPs) declPs'
              return [impPs']
        in  (anns', ast')
      -- TODO:: unised hidings. Leave as a noop for now
      Just (True, _) -> (anns, [impPs])
    MinimiseImports    -> (anns, [])  -- Drop the import completely
    NoImportProcessing -> (anns, [impPs])
  | not (null used)
  = case action of
    NoImportProcessing -> (anns, [impPs])
    _                  -> case ideclHiding declRn of
      Nothing ->
        let
          (ast', (anns', _n), _s) = runTransform anns $ do
            let names = map gre_name used
            importList <- mapM mkLIEVarFromNameT names
            unless (null importList) $ mapM_ addCommaT (init importList)
            let lImportList = L (RealSrcSpan locPs) importList -- locPS or unique?
                declPs'     = declPs { ideclHiding = Just (False, lImportList) }  -- assumes declPs is not an XImportDecl
            addParensT lImportList
            let impPs' = L (RealSrcSpan locPs) declPs'
            return [impPs']
        in  (anns', ast')

      Just (False, L locLIE lIEsRn) ->
        let
          Just (False, L _ lIEsPs) = ideclHiding declPs
          (usedImportsPs, anns')   = usedLImportDeclsPs lIEsPs lIEsRn
          declPs' =
            declPs { ideclHiding = Just (False, L locLIE usedImportsPs) }
          impPs' = L (RealSrcSpan locPs) declPs'
        in
          (anns', [impPs'])
      -- TODO:: unused hidings. Leave as a noop for now
      Just (True, _) -> (anns, [impPs])
 where

  -- TODO:: turn into a fold, or use monoid to make less ugly

  usedLImportDeclsPs :: [LIE GhcPs] -> [LIE GhcRn] -> ([LIE GhcPs], Anns)
  usedLImportDeclsPs lIEsPs lIEsRn = removeTrailingComma
    (concat lIEsPs', anns')
   where
    (lIEsPs', anns') = usedLImportDeclsPss anns lIEsPs lIEsRn

    removeTrailingComma :: ([LIE GhcPs], Anns) -> ([LIE GhcPs], Anns)
    removeTrailingComma ([]  , anns) = ([], anns)
    removeTrailingComma (lIEs, anns) = (lIEs', anns')
     where
      (lIEs', (anns', _), _) = runTransform anns $ do
        removeTrailingCommaT (last lIEs)
        setEntryDPT (head lIEs) (DP (0, 0))
        return lIEs

    usedLImportDeclsPss
      :: Anns -> [LIE GhcPs] -> [LIE GhcRn] -> ([[LIE GhcPs]], Anns)
    usedLImportDeclsPss anns [] [] = ([], anns)
    usedLImportDeclsPss anns (liePs : lIEsPs) (lieRn : lIEsRn) =
      let (lIEsPs', anns' ) = usedLImportDeclsPss anns lIEsPs lIEsRn
          (liePs' , anns'') = usedLImportDeclPs anns' liePs lieRn
      in  (liePs' : lIEsPs', anns'')


    usedLImportDeclPs :: Anns -> LIE GhcPs -> LIE GhcRn -> ([LIE GhcPs], Anns)
    usedLImportDeclPs anns liePs lieRn =
      if ieName (unLoc lieRn) `elem` map gre_name used -- TODO: factor this out
        then
--      let (ast', (anns', _), s) = runTransform anns $ do
--            -- Superfluous?
--            removeTrailingCommaT liePs
--            removeLocatedKeywordT (G GHC.AnnVal) liePs
--            return []
--      in   ([], anns')
             ([liePs], anns)
        else ([], anns)

