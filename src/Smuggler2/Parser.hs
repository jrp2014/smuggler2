{-# LANGUAGE CPP #-}
-- |
-- Description: provides a wrapper around the 'ghc-exactprint' parser

module Smuggler2.Parser
  ( runParser,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DynFlags (DynFlags, GeneralFlag (Opt_KeepRawTokenStream), gopt_set)
import GHC (ParsedSource)
import Language.Haskell.GHC.ExactPrint (Anns)
import Language.Haskell.GHC.ExactPrint.Parsers (parseModuleFromStringInternal)
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
import ErrUtils (fatalErrorMsg, printBagOfErrors )
import Outputable (text)
#else
import ErrUtils (fatalErrorMsg)
import Outputable (ppr, showSDoc, text)
#endif
import TcRnTypes (RnM)

-- | Wrapper around the 'ghc-exactprint' parser.  Prints diagnostics for failed parses
-- (which should never happen). We need to use
-- 'Language.Haskell.GHC.ExactPrint.parseModuleFromStringInternal'
-- because 'Language.Haskell.GHC.ExactPrintparseModuleFromString'
-- doesn't pick up the correct 'DynFlags' in some cases.
runParser ::
  DynFlags -> FilePath -> String -> RnM (Either () (Anns, ParsedSource))
runParser dflags fileName fileContents = do
  -- Without the following, comments are stripped (see #10942)
  -- It would be more efficient, but less visible, to apply this tweak at the
  -- outset, in the main plugin function, but keep it here for visibility
  -- See also https://gitlab.haskell.org/ghc/ghc/-/wikis/api-annotations, which
  -- notes that the flags are returned as annotations by the
  -- @Opt_KeepRawTokenStream@ flag.
  let dflags' = dflags `gopt_set` Opt_KeepRawTokenStream

  case parseModuleFromStringInternal dflags' fileName fileContents of
    Left msg -> liftIO $ do
#if MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)
      fatalErrorMsg dflags (text "smuggler2 parse failure:")
      printBagOfErrors dflags msg
#else
      fatalErrorMsg dflags (text $ "smuggler2 parse failure: " ++
                            showSDoc dflags (ppr $ fst msg) ++ ": " ++ snd msg)
#endif
      return $ Left ()
    Right x -> return $ Right x
