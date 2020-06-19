{-# LANGUAGE CPP #-}
-- |
-- Description:  A replacement for 'RnNames.getMinimalImports' that attempts
--               to handle patterns and types, which is not done correctly in GHC 8.10.1 and
--               earlier.
module Smuggler2.Imports (getMinimalImports) where

import Avail ( AvailInfo(..) )
import BasicTypes ( StringLiteral(sl_fs) )
import FieldLabel ( FieldLbl(flIsOverloaded, flLabel, flSelector) )
import GHC
    ( GhcRn,
      IE(IEThingAbs, IEThingAll, IEThingWith, IEVar),
      IEWildcard(NoIEWildcard),
      IEWrappedName(IEName, IEPattern, IEType),
      ImportDecl(ImportDecl, ideclHiding, ideclName, ideclPkgQual,
                 ideclSource),
      LIEWrappedName,
      LImportDecl )
import HscTypes -- earlier versions of Ghc don't have ModIface_
import LoadIface ( loadSrcInterface )
import Name ( HasOccName(..), isDataOcc, isTcOcc )
import Outputable ( Outputable(ppr), text, (<+>) )
#if MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
import RdrName ( gresToAvailInfo )
#endif
import RnNames ( ImportDeclUsage )
import SrcLoc ( GenLocated(L), Located, noLoc )
import TcRnMonad ( RnM )
import Language.Haskell.GHC.ExactPrint.Types ( noExt )

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
#if MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
           -- TODO not sure when this was introduced
           ; let used_avails = gresToAvailInfo used_gres
                 lies = map (L l) (concatMap (to_ie iface) used_avails)
#else
           -- used_gres are actually already AvailInfo in earlier versions of
           -- GHC
           ; let lies = map (L l) (concatMap (to_ie iface) used_gres)
#endif
           ; return (L l (decl { ideclHiding = Just (False, L l lies) })) }
      where
        doc = text "Compute minimal imports for" <+> ppr decl

    to_ie :: ModIface -> AvailInfo -> [IE GhcRn]
    -- The main trick here is that if we're importing all the constructors
    -- we want to say "T(..)", but if we're importing only a subset we want
    -- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie _ (Avail n) -- An ordinary identifier (eg, var, data constructor)
       = [IEVar noExt (to_ie_post_rn_var $ noLoc n)]
    to_ie _ (AvailTC n [m] []) -- type or class with absent () list
       | n==m = [IEThingAbs noExt (to_ie_post_rn_name $ noLoc n)]
    to_ie iface (AvailTC n ns fs)
      = case [(xs,gs) |  AvailTC x xs gs <- mi_exports iface
                 , x == n
                 , x `elem` xs    -- Note [Partial export]
                 ] of
           -- class / type with methods / constructors s
           [xs] | all_used xs -> [IEThingAll noExt (to_ie_post_rn_name $ noLoc n)] -- (..)

                | isTcOcc (occName n) -> -- typeclass -- @class Functor ...
                   [IEThingWith noExt (to_ie_post_rn_name $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn_varn . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]
                                          -- Note [Overloaded field import]

                | otherwise   -> -- type constructor (ie, @data X =@)
                   [IEThingWith noExt (to_ie_post_rn_name $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn_cname . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]

           -- record type
           _other | all_non_overloaded fs
                           -> map (IEVar noExt . to_ie_post_rn_name . noLoc) $ ns
                                 ++ map flSelector fs
                  | otherwise -> -- DuplicateRecordFields is applicable
                      [IEThingWith noExt (to_ie_post_rn_name $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn_cname . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]
        where

          fld_lbls = map flLabel fs

          all_used (avail_occs, avail_flds)
              = all (`elem` ns) avail_occs
                    && all ((`elem` fld_lbls) . flLabel) avail_flds

          all_non_overloaded = not . any flIsOverloaded

to_ie_post_rn_name :: Located name -> LIEWrappedName name
to_ie_post_rn_name (L l n) = L l (IEName (L l n))

to_ie_post_rn_var :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn_var (L l n)
  | isDataOcc $ occName n = L l (IEPattern (L l n))
  | otherwise = L l (IEName (L l n))

to_ie_post_rn_varn :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn_varn (L l n)
  | isTcOcc $ occName n = L l (IEType (L l n))
  | otherwise = L l (IEName (L l n))

to_ie_post_rn_cname :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn_cname (L l n)
  | isTcOcc $ occName n = L l (IEType (L l n))
  | otherwise = L l (IEName (L l n))

-- Notes
--
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/associating-synonyms

-- An import is
-- a var
-- a tycls -> [(..) | ( var1 , … , varn )]          -- class, etc
-- a tycon -> [ (..) | ( cname1 , … , cnamen )]     -- data

-- cname -> var | con
-- var -> varid | ( varsym )  -- (does not start with :)
-- con -> conid | ( consym )  -- (starts with :)

-- GHC User Guide 8.7.3
-- The name of the pattern synonym is in the same namespace as proper data constructors.
-- Like normal data constructors, pattern synonyms can be imported through associations
-- with a type constructor or independently.
-- To export them *on their own*, in an export or import specification,
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

-- isVarOcc -> variable name
-- isTvOcc -> is type variable
-- isTcOcc -> is type class name
-- isValOcc -- either in the variable or data constructor namespaces
-- isDataOcc -- Data constructor
-- isDataSymOcc -> Data contructuctor starting with a symbol
-- isSymOcc -> operator(data constructor, variable, etc)
--
-- So there are
-- var -> IEName
-- tycon -> can have IEPattern
-- tyclas -> can have IEType type (:+:)
