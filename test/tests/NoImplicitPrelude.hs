{-# LANGUAGE NoImplicitPrelude #-}

module NoImplicitPrelude where

import Data.List 
import Data.Version (makeVersion)
import Prelude

export = intercalate ", " ["Lorem", "ipsum", "dolor"]

version = makeVersion [1, 2, 3]

useprelude = "abc" <> "def"

useprelude2 = "efg" ++ "ghi"
