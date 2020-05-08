module Smuggler.Parser
  ( runParser
  , runImportsParser
  )
where

import           Language.Haskell.GHC.ExactPrint
                                                ( Anns, mergeAnnList )
import           Language.Haskell.GHC.ExactPrint.Parsers
                                                ( parseModuleFromString, parseImport, initDynFlagsPure)
import           HsExtension                    ( GhcPs )
import           HsSyn                          ( HsModule(..), LImportDecl(..) )
import           SrcLoc                         ( Located )
import DynFlags
import Control.Monad.IO.Class ( liftIO )
import ErrUtils


runParser
  :: FilePath -> String -> IO (Either () (Anns, Located (HsModule GhcPs)))
runParser fileName fileContents = do
  res <- parseModuleFromString fileName fileContents
  pure $ case res of
    Left  _ -> Left () -- The error case type changed in 8.10
    Right x -> Right x

runImportsParser'
  :: DynFlags -> FilePath -> String -> Either ErrorMessages (Anns, [LImportDecl GhcPs])
runImportsParser' dflags fileName fileContents = do
  let imports = lines fileContents
  res <- mapM (parseImport dflags fileName) imports

  let anns = fst <$> res
  let decls = snd <$> res

  Right (mergeAnnList anns, decls)


runImportsParser
  :: DynFlags -> FilePath -> String -> IO (Either ErrorMessages (Anns, [LImportDecl GhcPs]))
runImportsParser dflags fileName fileContents = do
  return $ runImportsParser' dflags fileName fileContents


{-
  res <- parseModuleFromString fileName fileContents
  pure $ case res of
    Left  _ -> Left () -- The error case type changed in 8.10
    Right x -> Right x

-- newtype ParseException = ParseException String
--     deriving (Show)
--
-- instance Exception ParseException
-- -}
