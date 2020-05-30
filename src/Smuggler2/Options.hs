{- |
 Description: handling of command line options

 -}
module Smuggler2.Options
  ( Options (..),
    parseCommandLineOptions,
    ImportAction (..),
    ExportAction (..),
  )
where

import Data.Char (toLower)
import Data.List (foldl')
import Plugins (CommandLineOption)

-- | Ways of performing import processing
data ImportAction = NoImportProcessing | PreserveInstanceImports | MinimiseImports
  deriving (Eq, Show)

-- | Ways of performing emport processing
data ExportAction = NoExportProcessing | AddExplicitExports | ReplaceExports
  deriving (Eq, Show)

-- | Internal representation of the plugin's command line options
data Options = Options
  { importAction :: ImportAction,
    exportAction :: ExportAction,
    newExtension :: Maybe String
  }
  deriving (Show)

-- | The default is to retain instance-only imports (eg, Data.List () )
-- and add explict exports only if they are not already present
defaultOptions :: Options
defaultOptions = Options PreserveInstanceImports AddExplicitExports Nothing

-- | Simple command line option parser.  Last occurrence wins.
parseCommandLineOptions :: [CommandLineOption] -> Options
parseCommandLineOptions = foldl' parseCommandLineOption defaultOptions
  where
    parseCommandLineOption :: Options -> CommandLineOption -> Options
    parseCommandLineOption opts clo = case toLower <$> clo of
      "noimportprocessing" -> opts {importAction = NoImportProcessing}
      "preserveinstanceimports" -> opts {importAction = PreserveInstanceImports}
      "minimiseimports" -> opts {importAction = MinimiseImports}
      "noexportprocessing" -> opts {exportAction = NoExportProcessing}
      "addexplicitexports" -> opts {exportAction = AddExplicitExports}
      "replaceexports" -> opts {exportAction = ReplaceExports}
      _ -> opts {newExtension = Just clo}