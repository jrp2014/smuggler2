#!/bin/bash

set -x

rm tmp/*.{o,hi}

cabal build

cabal exec  -- ghc \
  -fplugin Smuggler2.Plugin \
   -fplugin-opt=Smuggler2.Plugin:new \
   -fplugin-opt=Smuggler2.Plugin:MinimiseImports \
   -fplugin-opt=Smuggler2.Plugin:ReplaceExports \
   -fplugin-opt=Smuggler2.Plugin:LeaveOpenImports:Prelude \
   tmp/*.hs


