{-# LANGUAGE CPP #-}

module Smuggler2.Parser
  ( runParser,
  )
where

import DynFlags
    ( DynFlags, GeneralFlag(Opt_KeepRawTokenStream), gopt_set )
import ErrUtils ( fatalErrorMsg, printBagOfErrors )
import GHC ( ParsedSource )
import Language.Haskell.GHC.ExactPrint ( Anns )
import Language.Haskell.GHC.ExactPrint.Parsers
    ( parseModuleFromStringInternal )
import Outputable ( text )
import TcRnTypes ( RnM )
import Control.Monad.IO.Class ( MonadIO(liftIO) )


-- | Wrapper around the 'ghc-exactprint' parser.  Prints diagnostics for failed parses
-- (which should never happen). We need to use 'parseModuleFromStringInternal'
-- because 'parseMofuleFromString' doesn't pick up the correct 'DynFlags' in
-- some cases.
runParser ::
  DynFlags -> FilePath -> String -> RnM (Either () (Anns, ParsedSource))
runParser dflags fileName fileContents = do
  -- Withoout the following, comments are stripped (see #10942)
  -- It would be more efficient, but less visible to apply this tweak at the
  -- outset, in the main plugin function, but keep it here for visibility
  let dflags' = dflags `gopt_set` Opt_KeepRawTokenStream

  case parseModuleFromStringInternal dflags' fileName fileContents of
    Left msg -> liftIO $ do
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
      fatalErrorMsg dflags (text "smuggler parse failure:")
      printBagOfErrors dflags msg
#else
      fatalErrorMsg dflags (text $ "smuggler parse failure: " ++ msg)
#endif
      return $ Left ()
    Right x -> return $ Right x
