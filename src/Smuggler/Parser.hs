{-# LANGUAGE CPP #-}
module Smuggler.Parser
  ( runParser
  )
where

import Language.Haskell.GHC.ExactPrint ( Anns )
import Language.Haskell.GHC.ExactPrint.Parsers
    ( parseModuleFromString )
import GHC ( GhcPs, HsModule )
import SrcLoc ( Located )
import DynFlags ( DynFlags )
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
import ErrUtils ( printBagOfErrors )
#endif

runParser
  :: DynFlags -> FilePath -> String -> IO (Either () (Anns, Located (HsModule GhcPs)))
runParser dflags fileName fileContents = do
  res <- parseModuleFromString fileName fileContents
  case res of
    Left msg -> do
      putStr "smuggler: "
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
      printBagOfErrors dflags msg
#else
      print msg
#endif
      return $ Left ()
    Right x -> return $ Right x
