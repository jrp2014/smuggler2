-- | Description:  A replacement for 'RnNames.getMinimalImports' that attempts
-- to handle patterns and types, which is not done correctly in GHC 8.10.1 and
-- earlier.
module Smuggler2.Imports (getMinimalImports) where

import Avail (AvailInfo (..))
import BasicTypes (StringLiteral (sl_fs))
import FieldLabel (FieldLbl (flIsOverloaded, flLabel, flSelector))
import GHC.Hs (GhcRn, IE (IEThingAbs, IEThingAll, IEThingWith, IEVar), IEWildcard (NoIEWildcard),
               IEWrappedName (IEName, IEPattern, IEType),
               ImportDecl (ImportDecl, ideclHiding, ideclName, ideclPkgQual, ideclSource),
               LIEWrappedName, LImportDecl, noExtField)
import HscTypes (ModIface, ModIface_ (mi_exports))
import LoadIface (loadSrcInterface)
import Name (HasOccName (..), isDataOcc, isSymOcc, isTcOcc)
import Outputable (Outputable (ppr), text, (<+>))
import RdrName (gresToAvailInfo)
import RnNames (ImportDeclUsage)
import SrcLoc (GenLocated (L), Located, noLoc)
import TcRnMonad (RnM)

{-
Note [Partial export]
~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   module A( op ) where
     class C a where
       op :: a -> a

   module B where
   import A
   f = ..op...

Then the minimal import for module B is
   import A( op )
not
   import A( C( op ) )
which we would usually generate if C was exported from B.  Hence
the (x `elem` xs) test when deciding what to generate.


Note [Overloaded field import]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On the other hand, if we have

    {-# LANGUAGE DuplicateRecordFields #-}
    module A where
      data T = MkT { foo :: Int }

    module B where
      import A
      f = ...foo...

then the minimal import for module B must be
    import A ( T(foo) )
because when DuplicateRecordFields is enabled, field selectors are
not in scope without their enclosing datatype.

-}

-- | Attempt to fix
-- <https://hackage.haskell.org/package/ghc/docs/RnNames.html#v:getMinimalImports>
getMinimalImports :: [ImportDeclUsage] -> RnM [LImportDecl GhcRn]
getMinimalImports = mapM mk_minimal
  where
    mk_minimal (L l decl, used_gres, unused)
      | null unused
      , Just (False, _) <- ideclHiding decl
      = return (L l decl)
      | otherwise
      = do { let ImportDecl { ideclName    = L _ mod_name
                            , ideclSource  = is_boot
                            , ideclPkgQual = mb_pkg } = decl
           ; iface <- loadSrcInterface doc mod_name is_boot (fmap sl_fs mb_pkg)
           ; let used_avails = gresToAvailInfo used_gres
                 lies = map (L l) (concatMap (to_ie iface) used_avails)
           ; return (L l (decl { ideclHiding = Just (False, L l lies) })) }
      where
        doc = text "Compute minimal imports for" <+> ppr decl

    to_ie :: ModIface -> AvailInfo -> [IE GhcRn]
    -- The main trick here is that if we're importing all the constructors
    -- we want to say "T(..)", but if we're importing only a subset we want
    -- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie _ (Avail n) -- An ordinary identifier (eg,  var, data constructor)
       = [IEVar noExtField (to_ie_post_rn_var $ noLoc n)]
    to_ie _ (AvailTC n [m] []) -- type or class with absent () list
       | n==m = [IEThingAbs noExtField (to_ie_post_rn $ noLoc n)]
    to_ie iface (AvailTC n ns fs)
      = case [(xs,gs) |  AvailTC x xs gs <- mi_exports iface
                 , x == n
                 , x `elem` xs    -- Note [Partial export]
                 ] of
           -- class / type with methods / constructors
           [xs] | all_used xs -> [IEThingAll noExtField (to_ie_post_rn_var $ noLoc n)] -- (..)

                | isTcOcc (occName n) -> -- class
                   [IEThingWith noExtField (to_ie_post_rn $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn_tc . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]
                                          -- Note [Overloaded field import]

                | otherwise   -> -- type
                   [IEThingWith noExtField (to_ie_post_rn $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]

           -- record type
           _other | all_non_overloaded fs
                           -> map (IEVar noExtField . to_ie_post_rn . noLoc) $ ns
                                 ++ map flSelector fs
                  | otherwise -> -- DuplicateRecordFields is applicable
                      [IEThingWith noExtField (to_ie_post_rn $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]
        where

          fld_lbls = map flLabel fs

          all_used (avail_occs, avail_flds)
              = all (`elem` ns) avail_occs
                    && all ((`elem` fld_lbls) . flLabel) avail_flds

          all_non_overloaded = not . any flIsOverloaded


-- An import is
-- a var
-- a tycon -> [ (..) | ( cname1 , … , cnamen )]
-- a tycls -> [(..) | ( var1 , … , varn )]

-- cname -> var | con
-- var -> varid | ( varsym )  -- (does not start with :)
-- con -> conid | ( consym )  -- (starts with :)

-- GHC User Guide 8.7.3
-- The name of the pattern synonym is in the same namespace as proper data constructors.
-- Like normal data constructors, pattern synonyms can be imported through associations
-- with a type constructor or independently.
-- To export them on their own, in an export or import specification,
-- you must prefix pattern names with the pattern keyword
--
-- GHC User Guide 9.9.5
--The form C(.., mi, .., type Tj, ..), where C is a class, names the class C, and the
--specified methods mi and associated types Tj. The types need a keyword “type” to distinguish
--them from data constructors.
--
--Whenever there is no export list and a data instance is defined, the corresponding
--data family type constructor is exported along with the new data constructors, regardless of
--whether the data family is defined locally or in another module.

to_ie_post_rn_var :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn_var (L l n)
  | isDataOcc $ occName n = L l (IEPattern (L l n))
  | otherwise             = L l (IEName    (L l n))

to_ie_post_rn :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn (L l n)
  | isTcOcc occ && isSymOcc occ = L l (IEType (L l n))  -- starts with :, ->, etc
  | otherwise                   = L l (IEName (L l n))
  where occ = occName n

to_ie_post_rn_tc :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn_tc (L l n)
  | isTcOcc occ = L l (IEType (L l n))
  | otherwise   = L l (IEName (L l n))
  where occ = occName n
