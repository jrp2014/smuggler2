{-# LANGUAGE CPP #-}
module Smuggler.Parser
  ( runParser
  )
where

import Language.Haskell.GHC.ExactPrint ( Anns )
import Language.Haskell.GHC.ExactPrint.Parsers
    ( parseModuleFromString )
import HsExtension ( GhcPs )
import HsSyn ( HsModule )
import SrcLoc ( Located )
import DynFlags ( DynFlags )
import ErrUtils ( printBagOfErrors )

runParser
  :: DynFlags -> FilePath -> String -> IO (Either () (Anns, Located (HsModule GhcPs)))
runParser dflags fileName fileContents = do
  res <- parseModuleFromString fileName fileContents
  case res of
    Left msg -> do
#if __GLASGOW_HASKELL__ < 810
      show msg
      return $ Left ()
#else
      printBagOfErrors dflags msg
      return $ Left ()
#endif
    Right x -> return $ Right x
