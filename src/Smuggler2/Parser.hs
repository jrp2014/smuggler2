{-# LANGUAGE CPP #-}

module Smuggler2.Parser
  ( runParser,
  )
where

import DynFlags (DynFlags (..))
import ErrUtils (logOutput, printBagOfErrors)
import GHC (ParsedSource)
import Language.Haskell.GHC.ExactPrint (Anns)
import Language.Haskell.GHC.ExactPrint.Parsers (parseModuleFromStringInternal)
import Outputable (defaultUserStyle, text)

-- | Wrapper around the 'ghc-exactprint' parser.  Prints diagnostics for failed parses
-- (which should never happen). We need to use 'parseModuleFromStringInternal'
-- because 'parseMofuleFromString' doesn't pick up the correct 'DynFlags' in
-- some cases.
runParser ::
  DynFlags -> FilePath -> String -> IO (Either () (Anns, ParsedSource))
runParser dflags fileName fileContents = do
  let res = parseModuleFromStringInternal dflags fileName fileContents
  case res of
    Left msg -> do
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
      logOutput dflags (defaultUserStyle dflags)  (text "smuggler parse failure:")
      printBagOfErrors dflags msg
#else
      logOutput dflags (defaultUserStyle dflags)  (text "smuggler parse failure: " ++ msg)
#endif
      return $ Left ()
    Right x -> return $ Right x
